---
title: 컴파일 에러 대처 방법
category: Appendix
order: 2
---

## 전처리(ofasmpp) 에러

### 1001 : INPUT FILE NOT FOUND

* 입력 파일을 찾을 수 없음
* 파일 이름을 올바로 입력했는지 확인한다.

### 1002 : CICS MACRO FOUND. USE --enable-cics TO ENABLE CICS PREPROCESSING

* CICS 구문이 소스 파일 내에 사용되고 있다면, --enable-cics 옵션을 사용해야한다.

### 1004 : MACRO NOT FOUND

* 매크로 정의를 찾을 수 없음
* 해당 매크로가 정의된 파일을 컴파일 하고자 하는 파일과 동일한 디렉토리에 위치 시키거나 환경변수 $OFASM_MACLIB에 정의된 디렉토리에 위치시킨다.

### 1005 : COPYBOOK NOT FOUND

* 카피북을 찾을 수 없음
* 카피북 파일을 컴파일 하고자 하는 파일과 동일한 디렉토리에 위치 시키거나 환경변수 $OFASM_MACLIB에 정의된 디렉토리에 위치시킨다.


## 어셈블리(ofasma) 에러

### 2000 : NOT SUPPORTED INSTRUCTION

* OFASM에서 지원하지 않는 명령어가 사용됨

<pre>
...
* ACONTROL은 OFASM에서 지원하지 않는 명령어이다
           ACONTROL  AFPR,NOAFPR
...
</pre>

* OFASM의 제품 특성 상 지원이 필요없는 명령어인 경우(특히 Listing 관련), 주석 처리를 하고 재컴파일 하거나, 연구소에 검토 요청을 한다.

### 2001 : OPERAND HAS EMPTY EXPRESSION

* 명령어 수행에 필요한 피연산자가 없거나 그 개수가 적음

<pre>
...
           LR       12
...
</pre>

* 명령어의 포맷에 맞게 코드를 수정한다

<pre>
...
* LR은 RR 포맷이므로 레지스터 2개가 필요하다
           LR       12,15
...
</pre>

### 2002 : OPERAND HAVE INVALID EXPRESSION

* 피연산자의 포맷이 잘못됨

<pre>
...
           CHI       5(0,1),X'0001'
...
</pre>

* 명령어의 포맷에 맞게 코드를 수정한다

<pre>
...
* CHI 명령어는 RI 포맷이므로 첫번째 피연산자에는 R형태의 피연산자를 사용해야한다
           CHI       1,X'0001'
...
</pre>

### 2003 : BASE REGISTER IS NOT SET FOR GIVEN SECTION

* 주어진 Section에 대한 Base register가 설정되지 않음

<pre>
...
PARM        DSECT
P1          DS      CL15
P2          DS      CL4
P3          DS      CL4
TEST        CSECT
            LR          12,15
            USING       TEST,12
* PARM dummy section에 대한 Base register가 설정되지 않았는데 해당 section 내에 존재하는 P1 심볼을 사용
            OFADBGMEM   P1,2
...
</pre>

* USING을 사용하여 해당 섹션에 대한 Base register를 설정하는 부분이 없는지 찾아본다. 만약 없다면, 해당 섹션에 대한 Base register를 설정한다.

<pre>
...
PARM        DSECT
P1          DS      CL15
P2          DS      CL4
P3          DS      CL4
TEST        CSECT
            LR          12,15
            USING       TEST,12
* PARM dummy section에 대한 Base register를 설정한다
            USING       PARM,2
...
            OFADBGMEM   P1,2
...
</pre>

### 2005 : SYMBOL NOT FOUND

* 사용하고자 하는 심볼을 소스 내에서 찾을 수 없음

<pre>
E2005      CSECT
           LR        12,15
           USING     E2005,12
* SYTEST 심볼은 정의되어있지 않음
           A         1,SYTEST
           BR        14
           END
</pre>

* 해당 심볼을 정의한다. 혹은 해당 심볼이 정의된 카피북/매크로가 누락되지는 않았는지 확인한다.

<pre>
E2005      CSECT
           LR        12,15
           USING     E2005,12
           A         1,SYTEST
           BR        14
SYTEST     DC        F'4'
           END
</pre>

### 2006 : LENGTH OF OPERATION1 IS NOT SET

* S 포맷을 사용하는 기계명령어의 피연산자에서 길이 정보가 누락되었음

<pre>
...
           LA        3,TEMPAREA
           MVC       0(3),=C'TEMP'
...
</pre>

* 원인이 되는 피연산자에 길이 정보를 추가한다.

<pre>
...
           LA        3,TEMPAREA
* MVC는 SSa 포맷이므로 첫 번째 피연산자에 길이정보가 함께 들어가야한다
           MVC       0(4,3),=C'TEMP'
...
</pre>

### 2011 : INVALID PACK DIGIT FOUND. ALL DIGITS MUST BE 0~9

* DC로 선언한 Packed-decimal 값이 잘못되었음

<pre>
...
INVPCK      DC      P'2BADF'
...
</pre>

* 올바른 Packed-decimal 값으로 수정한다

<pre>
...
* Packed-decimal은 부호(+,-)나 숫자(0~9)만 사용 가능하다
INVPCK      DC      P'-1234'
...
</pre>

### 2012 : INVALID BASE REGISTER NUMBER

* Base register의 값이 잘못되었음

<pre>
...
   USING    *,18
...
</pre>

* Base register의 값을 0~15 사이의 값으로 수정한다.

<pre>
...
    USING   *,12
...
</pre>

### 2014 : OPERAND LENGTH IS OVER 256

* 피연산자의 길이가 256을 초과함
<pre>
...
         MVC   WKAREA(1024),SPACE
...
</pre>

* S 포맷의 피연산자는 바이트 단위로 최대 256까지의 길이를 허용한다. 256 바이트가 넘어가는 데이터에 대해서는 명령어를 분리하여 사용해야한다.

<pre>
         MVC   WKAREA(256),SPACE    
         MVC   WKAREA+256(256),SPACE
         MVC   WKAREA+512(256),SPACE
         MVC   WKAREA+768(256),SPACE
</pre>

### 2016 : SYMBOL ALREADY DEFINED

* 같은 이름의 심볼이 이미 정의 되어있음

* 중복되는 심볼의 이름을 변경한다.

### 2017 : OPERAND LENGTH IS OVER 16

* 피연산자의 길이가 16을 초과함

<pre>
...
    PACK   DATA1,DATA2
...
DATA1      DC    ZL32'1000000000000000000000000000000000000000000000000X
               0000000000000000'
DATA2      DC    PL16
</pre>

* SS-b 포맷의 피연산자를 사용하는 명령어는 각 피연산자의 길이 정보는 최대 16까지만 허용한다. 16 바이트가 넘어가는 데이터에 대해서는 명령어를 분리하여 사용해야한다.

<pre>
...
    PACK   DATA1(16),DATA2
...
DATA1      DC    ZL32'1000000000000000000000000000000000000000000000000X
               0000000000000000'
DATA2      DC    PL16
</pre>

### 2018 : ODD REGISTER FOUND. MUST USE EVEN REGISTER.(Incorrect register specification)

* Even-odd 레지스터를 사용하는 명령어에 대해서 적절하지 않은 레지스터 번호(홀수 레지스터 번호)를 사용함

<pre>
...
    M   3,=F'4'
...
</pre>

* 해당 피연산자에 짝수 번호 레지스터를 사용하도록 한다.
<pre>
...
    M   2,=F'4'
...
</pre>

### 2019 : INVALID REGISTER NUMBER.(Incorrect register specification)

* 레지스터 번호가 올바르지 않음.

<pre>
...
    LR    2,17
...
</pre>

* 올바른 레지스터 번호(0~15)로 수정한다.

<pre>
...
    LR    2,13
...
</pre>

### 2021 : LENGTH MODIFIER MUST BE POSITIVE VALUE

* DC/DS 심볼 선언 시 사용되는 길이 정보가 양수가 아님

<pre>
...
LNG1    EQU     20
LNG2    EQU     30
* 아래 뺄셈의 결과값이 -10 이므로 에러가 발생한다.
        DS      XL(LNG1-LNG2)
...
</pre>

* 해당 길이 정보가 양수가 되도록 수정한다.
<pre>
...
LNG1    EQU     20
LNG2    EQU     30
        DS      XL(LNG2-LNG1)
...
</pre>

### 2023 : NOT SUPPORTED TYPE

* 지원하지 않는 데이터 타입
<pre>
...
* 현재 OFASM은 Q-type을 지원하지 않는다
DATA    DS      QL4
...
</pre>

* 지원하는 데이터 타입이므로, 해당 구문을 수정하거나 연구소측으로 문의한다

### 2024 : ONLY SUPPORT DEPENDENT USING IN DUMMY SECTION

* Dependent USING에 대해서는 현재 DSECT에 대해서만 지원하고 있음

* 연구소측으로 문의 바람

### 2025 : MULTIPLE DATA EXPRESSION FOUND IN ANALYZER

//TBD

### 2026 : NOT SUPPORTED INSTRUCTION IN COMMON SECTION

//TBD

### 2027 : PROGRAM TYPE AND ASSEMBLER TYPE IS NOT SUPPORTED

* EQU instruction에서 사용할 수 있는 피연산자 중 program type 과 assembler type은 OFASM에서 지원하지 않음을 표시함

* 해당 에러 발생 시에는 연구소로 문의 바람

### 2028 : INVALID OPERAND EXPRESSION TYPE

* 적절하지 않은 피연산자 표현식이 사용되었을 때

### 2029 : NOT IMPLEMENTED

* 지원하지 않는 타입, 기능 등을 포괄적으로 나타냄

### 2030 : PROGRAM SIZE IS TOO BIG

* 프로그램 사이즈가 너무 큼
* OFASM은 프로그램 사이즈를 128 mb (0x08000000 bytes) 로 제한하고 있다.

### 2033 : EXTERNAL SYMBOL IS NOT SUPPORTED ON S-type DC/DS

* S type의 데이터에 대해서 외부 심볼에 대한 정의를 현재 지원하지 않는다.

### 2035 : NO SYMBOL FOUND FOR USING

* USING에 사용된 심볼을 찾을 수 없다.
<pre>
TEST        CSECT
            LR   12,15
            USING TEST,12
* TEMP 심볼은 정의되어 있지 않다
            USING TEMP,4
            BR    14
            END
</pre>

* 해당 심볼을 사용하는 섹션을 정의하거나, 사용되지 않는 USING 구문을 삭제한다.

### 2037 : INVALID RELOCATION COUNTER.

* relocation 관련 에러로, 해당 에러는 다양한 케이스가 존재하므로 연구소로 문의한다.

### 2038 : RELOCATION SYMBOL ALREAY EXIST

* relocation 관련 에러로, 해당 에러는 다양한 케이스가 존재하므로 연구소로 문의한다.

### 2039 : INVALID NUMBER

* EDIT 관련 명령어(ED, EDMK)에서 발생하는 에러로, 첫 번째 피연산자가 올바르지 않은 Packed-decimal 데이터일 때 발생한다.
* 첫 번째 피연산자의 데이터를 확인한다.