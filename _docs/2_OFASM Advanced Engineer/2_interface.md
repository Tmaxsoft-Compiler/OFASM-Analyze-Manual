---
title: 인터페이스 작성 방법
category: OFASM Advanced Engineer
order: 2
---

## 인터페이스 작성 방법

### OFASM 인터페이스의 필요성

OFASM은 리눅스 환경 위에 메인프레임과 유사한 메모리 구조를 가지는 가상 머신(VM: Virtual Machine) 을 생성하고, 해당 메모리에 OFASM 프로그램(asmo 파일)을 로드(Load)하여 동작을 수행한다. OFASM 가상 머신의 구조는 자바 가상 머신의 모델을 본 따 설계된 것이기 때문에 외부에서 OFASM 프로그램을 호출하거나, OFASM에서 외부 프로그램을 호출하기 위해서는 자바에서 사용하는 JNI처럼 인터페이스 프로그램이 필요하다.
즉, OFASM과 외부 프로그램 사이에는 각 호출 구조에 따라 세 가지의 인터페이스 프로그램이 필요하다. 

1. 외부 프로그램(COBOL 혹은 PLI)이 OFASM 프로그램을 호출
    * vmEntry를 호출하는 Entry 인터페이스 프로그램(XXX.so 파일)이 필요
2. OFASM 프로그램이 외부 프로그램을 호출
    * 외부 프로그램을 직접 호출하는 Exit 인터페이스 프로그램(XXX_OFASM_VM_EXIT.so)이 필요
3. CICS 환경에서 어셈블러 프로그램을 로드해놓고 사용하는 경우
    * CICS 환경에서 사용하는 Load 인터페이스 프로그램(XXX_OFASM_VM_LOAD.so)이 필요

단, OFASM 프로그램끼리 호출하는 경우에는 인터페이스가 필요하지 않다.

### Entry 인터페이스

#### OFASM이 전달받는 파라미터의 종류

##### 파라미터 리스트의 종류

OFASM 은 프로그램을 호출할 때 파라미터 리스트(Parameter List)를 생성하여 호출한다. OFASM에서 제공하는 파라미터 리스트의 종류는 아래와 같다.

1. 고정 개수 파라미터 리스트(Fixed-number Parameter List)
    * 파라미터 리스트가 포함하는 파라미터의 개수가 고정 되어 있음을 의미한다.
2. 가변 개수 파라미터 리스트(Variable-number Parameter List)
    * 파라미터 리스트가 포함하는 파라미터의 개수가 고정 되어 있지 않고 변동 될 수 있음을 의미한다. 이 때, 어셈블러 프로그램에서는 파라미터 리스트가 포함하고 있는 파라미터의 주소의 최상위 비트가 1인지 검사하는 방법으로 마지막 파라미터인지 아닌지 검사할 수 있다.

##### 파라미터의 종류

OFASM 프로그램의 파라미터 리스트가 포함할 수 있는 파라미터의 종류는 아래와 같다.

1. 고정 크기 파라미터(Fixed size parameter)
    * 하나의 파라미터의 크기가 고정되어있을 때 그것을 고정 크기 파라미터라고 지칭한다. 
2. 가변 크기 파라미터(Variable size parameter)
    * 하나의 파라미터의 크기가 정해져있지 않을 때 그것을 가변 크기 파라미터라고 지칭한다. 파라미터 리스트 내에서 이 파라미터를 가리키는 주소의 2바이트에 파라미터 크기 정보가 삽입된다. 보통 JCL에서 어셈브러를 직접적으로 호출할 때 이 파라미터를 사용한다.
3. 포인터 파라미터(Pointer parameter)
    * 하나의 파라미터가 다른 메모리 영역을 가리킬 때 그것을 포인터 파라미터라고 지칭한다.

#### JSON 파일 작성 방법

아래는 OFASM 인터페이스 파일을 생성하기 위한 JSON의 작성 예이다.

```json
{
"entry_list":
[
    {
        "entry_name" : "ENTRYNAME1",
        "fixed_parameter_list" : 
        [
            {
                "param_size" : (PARASIZE),
                "param_type" : (PARATYPE),
                "pointer_offset_list" : [OFFSET_POSITION, ...],
                "pointer_size_list" : [POINTER_SIZE, ...]
            },
        ]
    },
    {
        "entry_name" : "ENTRYNAME2",
        "variable_parameter_list" : 
        {
            "max_length" : (MAXLEN)
        }
    }
],
"program_name" : "PROGNAME",
"version" : 3,
"interface_type" : "entry"
}
```

각 JSON field에 관한 설명은 아래와 같다.

1.  "entry_list": JSONLIST
    - JSONLIST로 구성되어있다. 엔트리 이름을 나열한 엔트리 리스트를 나타낸다. 각 요소는 모두 "entry_name" 필드를 포함하며, "fixed_parameter_list" 또는 "variable_parameter_list" 필드를 포함한다.
2. "entry_name" : STRING
    - ASM 파일 내의 엔트리 이름을 정의한다. 인터페이스 파일을 생성하면 해당 이름을 가진 함수들이 작성된다. 
3.  "fixed_parameter_list": JSONLIST
    - JSONLIST로 구성되어있다. 고정 길이를 가지는 파라미터 리스트를 서술하기 위한 Json list이다. 각 요소는 모두 "param_type" 필드를 포함하며, 경우에 따라 "param_size"를 포함할 수 있다.
    - "param_type" : NP, F, V, P
      - 파라미터의 타입을 정의한다
        - NP 또는 F: 고정 길이 파라미터, 고정 길이 파라미터를 정의했다면 param_size를 반드시 지정해주어야한다.
        - V: 가변 길이 파라미터, 가변 길이 파라미터는 해당 파라미터가 저장된 주소의 최상단 2바이트에 크기가 지정되어야한다.
        - P: 포인터 타입 파라미터, 포인터 타입 파라미터는 단일 포인터 타입뿐만 아니라 포인터를 포함하고 있는 구조체에 대해서도 해당 타입을 지정해야한다. 포인터 타입 파라미터를 정의했다면, pointer_offset_list 와 pointer_size_list 를 반드시 지정해주어야 한다.  
    - "param_size" : INTEGER
      - 파라미터의 크기를 정의한다.
      - 파라미터가 없는 프로그램에 대해서는 아래와 같이 비어있는 fixed_parameter_list를 사용하도록 정의한다.
    ```json
    {
    ...
        "fixed_parameter_list" : []
    ...
    }
    ```
    - "pointer_offset_list" : JSONLIST
      - JSONLIST로 구성되어 있다. 파라미터 구조체 안에 포함되어 있는 포인터 offset 리스트를 서술하기 위한 json list이다. 해당 list 는 param_type이 "P"인 경우에만 필요하며, "pointer_size_list"도 함께 작성되어야 한다.
      - 구조체 내에서 포인터가 위치한 offset 값을 차례대로 정의한다.
    - "pointer_size_list" : JSONLIST
      - JSONLIST로 구성되어 있다. 파라미터 구조체 안에 포함되어 있는 포인터의 size 리스트를 서술하기 위한 json list이다. 해당 list 는 param_type이 "P"인 경우에만 필요하며, list의 갯수는 "pointer_offset_list" 에 정의되어 있는 갯수와 같아야 하며 쌍으로 이루어져야한다.
      - 구조체 내에서 해당 포인터의 size 정보를 차례대로 정의한다.
4. "variable_parameter_list" : JSONOBJECT
   - 하나의 Json object 필드를 포함한다. 가변 길이를 가지는 파라미터 리스트를 정의하기 위한 json object이다. 각 요소는 모두 "max_length"필드를 포함한다.
   - "max_length" : INTEGER
   - 가변 길이를 가지는 파라미터 리스트가 최대로 가질 수 있는 파라미터의 개수를 정의한다.
5. "program_name" : STRING
   - 프로그램 이름을 정의한다. 인터페이스 파일을 생성하면 이 필드에 정의된 이름 + “_OFASM_VM_ENTRY.cpp” 파일이 생성된다. 예를 들어 이 필드에 TEMPPROG라고 정의하면, 생성되는 인터페이스 파일의 이름은 TEMPPROG_OFASM_VM_ENTRY.cpp 이 된다.
6. "version" : 3
   - ofasmif의 버전을 나타낸다.
7. "interface_type" : STRING
   - 인터페이스의 타입을 정의한다. "interface_type" 을 정의하지 않으면, 기본적으로 entry 인터페이스를 생성한다. 정의할 수 있는 타입은 "entry", "load", "exit" 가 있다. 생성하고자 하는 인터페이스 타입을 정의해주면 해당 인터페이스를 생성하게 된다.

아래 커맨드를 입력하여 작성된 JSON 파일을 통해 인터페이스 파일(.cpp파일)을 생성할 수 있다.

<pre>
    ofasmif –i XXXX.json
</pre>

#### OFASM 파라미터 분석 방법
이러한 인터페이스를 만들기 위해서는 어셈블러 프로그램으로 넘겨지는 파라미터의 개수와 각 파라미터의 사이즈를 알아야 한다. 이를 분석하기 위한 방법으로는 아래 3가지 방법이 있다.

##### COBOL 파일에서 분석하는 방법

인터페이스를 만들고자 하는 프로그램을 호출하는 COBOL 소스 부분을 확인한다.

```cobol
CALL 'PROGGMT' USING TEMP-GMT.
```

###### 포인터 파라미터가 없는 경우

예를 들어 위와 같은 코볼 소스에서는 PROGGMT 어셈블러 프로그램을 호출할 때 TEMP-GMT라는 host variable을 파라미터로 사용한다는 것을 알 수 있다. 따라서 파라미터의 개수는 1개이며, 파라미터의 사이즈는 TEMP-GMT의 전체 사이즈인 것을 알 수 있다. Host variable인 TEMP-GMT는 아래와 같이 정의되어있다.

```cobol
SIZE***DATA************************************************************
       01  TEMP-GMT.
        12 GMT-PREFIX.
2           15 GMT-LL           PIC S9(4)  COMP   VALUE +28.
2           15 GMT-VERSION      PIC S9(4)  COMP   VALUE +1.
8           15 GMT-ID           PIC  X(8)         VALUE 'TEMPGMT '.
                88 GMT-ID-OK                       VALUE 'TEMPGMT '.
2           12 GMT-RC              PIC  9(4)  BINARY VALUE 0.
2           12 GMT-FC              PIC  XX           VALUE 'AT'.
                88 GMT-GET-ACTUAL-TIME                VALUE 'AT'.
                88 GMT-CONVERT-IAK-TS                 VALUE 'CI'.
8           12 GMT-TOD             PIC  9(18) BINARY VALUE 0.
8           12 GMT-MICSEC          PIC  9(18) BINARY VALUE 0.
8           12 GMT-MS-DEC          PIC  9(15) COMP-3 VALUE 0.
8           12 GMT-CICS-ABSTIME    PIC  9(15) COMP-3 VALUE 0.
7           12 GMT-HHMMSSTHMIJU    PIC  9(13) COMP-3 VALUE 0.
4           12 GMT-YYYYDDD         PIC  9(07) COMP-3 VALUE 0.
4           12 GMT-YYYYMMDD        PIC  9(08) COMP-3 VALUE 0.
6           12 GMT-HHMMSS          PIC  9(06)        VALUE 0.
2           12 GMT-MILLISEC        PIC  9(03) COMP-3 VALUE 0.
4           12 GMT-MICROSEC        PIC  9(06) COMP-3 VALUE 0.
            12 GMT-ED-TIMESTAMP.
11              15 GMT-DD-MM-YYYY   PIC  X(11)        VALUE ' '.
15              15 GMT-HH-MM-SS-MIC PIC  X(15)        VALUE ' '.
                15 FILLER           REDEFINES GMT-HH-MM-SS-MIC.
12                  18 GMT-HH-MM-SS-MIL PIC X(12).
                15 FILLER           REDEFINES GMT-HH-MM-SS-MIC.
8                   18 GMT-HH-MM-SS  PIC X(08).
8           12 GMT-IAK-TIMESTAMP   PIC X(08)         VALUE LOW-VALUE.
140         12 FILLER              PIC X(140)        VALUE LOW-VALUE.
```

각 라인의 사이즈가 첫 컬럼에 기술되어있다.

각 변수들이 차지하는 크기를 나열하고 전부 더해보면 269 바이트가 나온다. 따라서 TEMP-GMT 크기는 269 바이트이다.
따라서, 위에 기술된 프로그램 PROGGMT에 대한 인터페이스 파일을 만들기 위해 JSON 파일을 아래와 같이 작성할 수 있다.

```json
{
    "entry_list" : 
    [{
        "entry_name" : "PROGGMT",
        "fixed_parameter_list" : 
        [
            {
                "param_type" : "F",
                "param_size" : 269
            }
        ]
    }],
"program_name" : "PROGGMT",

"version" : 3
}
```

###### 포인터 파라미터가 있는 경우

아래와 같이 파라미터의 구조체에 포인터가 포함되어 있는 경우, 다음과 같은 방법으로 작성할 수 있다.

```cobol
01 TEMP-GMT.
   02 TEST-PARAMETERS.
      03 ARG1     PIC X(8) VALUE 'TESTAREA'.
      03 ARG2     PIC S9(8) COMP VALUE 2.
      03 ARG3     PIC S9(8) COMP VALUE 3.
      03 ARG4-PTR POINTER VALUE NULLS.
      03 ARG5-PTR POINTER VALUE NULLS.
      03 ARG6     PIC S9(8) COMP VALUE 6.
      03 ARG7     PIC X(8) VALUE 'ARGUMENT'.
      03 ARG8-PTR POINTER VALUE NULLS.
```
포인터 파라미터가 있는 경우는 각 포인터 타입의 offset 과 size 정보가 필요하다. offset 계산과 size 정보를 바탕으로 아래와 같이 JSON 파일을 작성할 수 있다.

```json
{
    "entry_list" : 
    [{
        "entry_name" : "ASMPTR",
        "fixed_parameter_list" : 
        [
            {
                "param_type" : "P",
                "param_size" : 40,
                "pointer_offset_list" : [ 16, 20, 36 ],
                "pointer_size_list" : [ 100, 200, 300]
            }
        ]
    }],
"program_name" : "ASMPTR",

"version" : 3
}
```
주의) JSON 파일 내의 pointer_offset_list 작성 시, 코볼의 포인터값은 4바이트로 상정하고 작성한다. pointer_size_list 는 각 pointer 가 가르키는 버퍼의 사이즈로 작성한다.

##### JCL 파일에서 분석하는 방법

JCL에서 호출하는 어셈블러에 대한 인터페이스는 언제나 고정 길이 파라미터 리스트(fixed_parameter_list)를 사용한다. JCL에서 프로그램을 호출할 때 파라미터를 사용하는지에 대한 여부는 EXEC PGM 구문에서 PARM 필드를 통해 판단할 수 있다. 우선 JCL에서 어셈블러 프로그램을 실행하는 부분을 확인한다.

<pre>
...
//STEP1     EXEC PGM=PGMTEMP
...
</pre>

우선 Table 8와 같이 PARM 필드를 사용하지 않고 어셈블러를 호출할 수도 있다. 이러한 경우에는 JSON 파일의 fixed_parameter_list 부분을 Table 2 를 참조하여 작성하면 된다.

두 번째로, 어셈블러 호출하는 EXEC PGM 구문에서 PARM 필드를 사용할 때는 보통 아래와 같이 JCL 파일이 작성되어있다. 

<pre>
...
//STEP1     EXEC PGM=PGMTEMP,PARM=’P1,123,MT5’
...
</pre>

이러한 경우에는 PGMTEMP 프로그램으로 P1,123,MT5 라는 파라미터가 전달되며, 해당 파라미터에 대한 길이 정보가 파라미터 주소의 상위 2바이트에 삽입되어 전달된다. 이러한 특성을 반영한 파라미터 타입이 V 타입인데, 이것을 이용하여 아래와 같이 JSON 파일을 작성한다.

``` json
{
    "entry_list" : 
    [{
        "entry_name" : "PROGTEMP",
        "fixed_parameter_list" : 
        [
            {
                "param_type" : "V"
            }
        ]
    }],
"program_name" : "PROGTEMP",

"version" : 3
}
```

##### 어셈블러 파일에서 분석하는 방법

위 두가지 방법으로 파라미터 길이 분석이 힘든 경우에는 본 방법을 사용해야 한다. 이 경우는 다양한 케이스가 존재하므로 연구소측으로 문의하도록 한다.

#### Entry 인터페이스 파일 컴파일

아래와 같은 g++ 커맨드를 입력하여 JSON 파일을 사용하여 생성된 인터페이스 파일(.cpp파일)을 통해 so 파일을 생성한다.

<pre>
g++ -shared –fPIC –o XXXX.so XXXX_OFASM_VM_ENTRY.cpp –L$OFASM_HOME/lib –lofasmVM
</pre>

### Exit 인터페이스

#### 파일 이름 규약

OFASM VM은 외부 프로그램을 호출하기 위해 파일 이름 및 함수 이름에 대한 규약을 가진다.
EXIT 인터페이스는 다음과 같은 JSON 파일을 통해 생성할 수도 있고, 직접 코드를 작성하여 생성할 수도 있다. 

```json
{
"entry_list":
[
    {
        "entry_name" : "ENTRYNAME1",
        "fixed_parameter_cnt" : (PARACNT)
    }
],
"program_name" : "PROGNAME",
"version" : 3,
"interface_type" : "exit"
}
```

- "fixed_parameter_cnt" (가칭) : 외부 프로그램(코볼 등)에서 파라미터를 사용하지 않는다면 0, 사용한다면 해당 파라미터의 갯수를 지정해준다.

마찬가지로 아래 커맨드를 입력하여 작성한 JSON 파일을 통해 Exit 인터페이스 파일(.cpp파일)을 생성할 수 있다.

<pre>
    ofasmif –i XXXX.json
</pre>

파라미터가 없는 TESTCOB 이라는 외부 프로그램을 호출하기 위한 Exit 인터페이스는 아래과 같이 생성된다.

``` c++
#include <stdlib.h>

extern "C"
{

extern int TESTCOB();

int TESTCOB_OFASM_VM_EXIT()
{
    int rc = TESTCOB();
    return rc;
}

}
```

또한, TESTCOB 프로그램이 3개의 파라미터를 받는다면 아래와 같이 Exit 인터페이스가 생성된다. 

``` c++
...
extern int TESTCOB(char* p0, char* p1, char* p2);

int TESTCOB_OFASM_VM_EXIT(char* p0, char* p1, char* p2)
{
    int rc = TESTCOB(p0, p1, p2);
    return rc;
}
...
```

정의된 함수의 이름에 주목하길 바란다. 함수의 이름은 반드시 *“호출할 프로그램의 이름 + _OFASM_VM_EXIT”* 가 되어야 한다. 이는 OFASM에서 Exit 인터페이스를 호출하기 위해 고유하게 사용하는 이름 규약이다. 

#### Exit 인터페이스 파일 컴파일

Exit 인터페이스 파일은 아래와 같은 명령어로 컴파일 한다.

<pre>
g++ -fPIC –shared –o TESTCOB_OFASM_VM_EXIT.so TESTCOB_OFASM_VM_EXIT.cpp  –L./ TESTCOB.so
</pre>

 출력 파일의 이름에도 역시 주목하길 바란다. Exit 인터페이스에 대한 함수 이름과 함께 파일 이름도 동일한 규약을 가진다. *“호출할 프로그램의 이름 + _OFASM_VM_EXIT.so”* 가 되어야 한다. 


### Load 인터페이스

Load 인터페이스 파일을 생성하기 위한 JSON 파일 작성 방법은 entry 인터페이스 생성을 위한 JSON 파일 작성 방법과 동일하며, 반드시 "interface_type" 을 "load"로 지정해주어야 한다.

```json
{
"entry_list":
[
    {
        "entry_name" : "ENTRYNAME1",
        "fixed_parameter_list" : 
        [
            {
                "param_size" : (PARASIZE),
                "param_type" : (PARATYPE),
                "pointer_offset_list" : [OFFSET_POSITION, ...],
                "pointer_size_list" : [POINTER_SIZE, ...]
            },
        ]
    }
],
"program_name" : "PROGNAME",
"version" : 3,
"interface_type" : "load"
}
```
Load 인터페이스 또한 Load 인터페이스에 대한 함수 이름과 함께 파일 이름도 동일한 규약을 가진다. *“호출할 프로그램의 이름 + _OFASM_VM_LOAD.so”* 가 되어야 한다.

예를 들어, 포인터 타입을 포함하는 경우 JSON 파일은 다음과 같이 작성된다.

```json
{
    "entry_list" :
    [
        {
            "entry_name" : "LTEST",
            "fixed_parameter_list" : [
            {
               "param_size" : 40,
               "param_type" : "P",
               "pointer_offset_list" : [ 16, 20, 36 ],
               "pointer_size_list" : [ 4, 4, 4 ]
            }
            ]
        }
    ],
    "program_name" : "LTEST",
    "version" : 3,
    "interface_type" : "load" 
}
```

JSON 파일을 통해 생성된 Load 인터페이스는 다음과 같다.

``` c++
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdio.h>

extern "C" 
{

/**
 *    @brief The LTEST_STRUCT_ASM struct
 */
struct __attribute__((packed)) LTEST_STRUCT_ASM
{
    uint8_t buffer0[16];
    uint32_t addr0;
    uint32_t addr1;
    uint8_t buffer2[12];
    uint32_t addr2;
};

/**
 *    @brief The LTEST_STRUCT_COB struct
 */
struct __attribute__((packed)) LTEST_STRUCT_COB
{
    uint8_t buffer0[16];
    char *addr0;
    char *addr1;
    uint8_t buffer2[12];
    char *addr2;
};

/**
 * @brief LTEST_OFASM_VM_LOAD_SIZE return byte size ofLTEST_STRUCT_COB
* @return byte size ofLTEST_STRUCT_COB (msut be 1 or higher)
 */
int LTEST_OFASM_VM_LOAD_SIZE()
{
    return sizeof(LTEST_STRUCT_COB);
}

/**
 * @brief LTEST_OFASM_VM_LOAD
 * @param asm_ptr address of LTEST_STRUCT_ASM
 * @param cob_ptr address of LTEST_STRUCT_COB
 * @param asm_size byte size of assembler file
 * @return 0: success, -1: error
 */
int LTEST_OFASM_VM_LOAD(char *asm_ptr, char *cob_ptr, int asm_size)
{
    int ltest_struct_asm_size = sizeof(LTEST_STRUCT_ASM);
    if(asm_size != ltest_struct_asm_size) {
        printf("LTEST_OFASM_VM_LOAD: LTEST.asm & LTEST_STRUCT_ASM SIZE IS NOT EQUAL\n");
        return -1;
    }

    LTEST_STRUCT_ASM* ltest_struct_asm_ptr = (LTEST_STRUCT_ASM*) asm_ptr;
    LTEST_STRUCT_COB* ltest_struct_cob_ptr = (LTEST_STRUCT_COB*) cob_ptr;

    memcpy(ltest_struct_cob_ptr->buffer0, ltest_struct_asm_ptr->buffer0, sizeof(ltest_struct_asm_ptr->buffer0));
    ltest_struct_cob_ptr->addr0 = ltest_struct_asm_ptr->addr0;
    ltest_struct_cob_ptr->addr1 = ltest_struct_asm_ptr->addr1;
    memcpy(ltest_struct_cob_ptr->buffer2, ltest_struct_asm_ptr->buffer2, sizeof(ltest_struct_asm_ptr->buffer2));
    ltest_struct_cob_ptr->addr2 = ltest_struct_asm_ptr->addr2;

    return rc;
}

}
```

#### Load 인터페이스 파일 컴파일

Load 인터페이스 파일은 아래와 같은 명령어로 컴파일 한다.

<pre>
g++ -fPIC –shared –o LTEST_OFASM_VM_LOAD.so LTEST_OFASM_VM_LOAD.cpp
</pre>

 출력 파일의 이름에도 역시 주목하길 바란다. Load 인터페이스에 대한 함수 이름과 함께 파일 이름도 동일한 규약을 가진다. *“호출할 프로그램의 이름 + _OFASM_VM_LOAD.so”* 가 되어야 한다. 
