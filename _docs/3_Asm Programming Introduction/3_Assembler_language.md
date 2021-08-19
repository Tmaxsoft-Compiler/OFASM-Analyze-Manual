---
title: Assembler 프로그래밍 언어
category: ASM Programming Introduction
order: 3
---

## 목차

- [목차](#목차)
- [Assembler language](#assembler-language)
  - [Machine instructions](#machine-instructions)
    - [Machine Instruction Formats](#machine-instruction-formats)
  - [Assembler instructions](#assembler-instructions)
    - [Program Control](#program-control)
    - [Program Section and Linking](#program-section-and-linking)
    - [Base Register](#base-register)
    - [Data Definition](#data-definition)
  - [Macro instructions](#macro-instructions)
    - [Model statements](#model-statements)
      - [Rules for concatenation](#rules-for-concatenation)
      - [Rules for model statement fields](#rules-for-model-statement-fields)
    - [Symbolic parameters](#symbolic-parameters)
    - [Processing statements](#processing-statements)
    - [Comment statements](#comment-statements)
    - [System variable symbols](#system-variable-symbols)

----

## Assembler language
어셈블러 언어는 형식과 내용 면에서 기계어에 가장 가까운 기호 프로그래밍 언어이다. 어셈블러 언어는 다음과 같은 경우에 유용하다.

* 바이트 및 비트 수준까지 프로그램을 밀접하게 제어해야 하는 경우
* COBOL, Fortran 또는 PL/I와 같은 다른 프로그래밍 언어에서 제공하지 않는 기능에 대한 서브루틴을 작성해야 하는 경우 

어셈블러 언어는 명령이나 주석을 나타내는 명령문으로 구성된다. Instruction은 언어의 작동 부분이며 다음 세 그룹으로 나뉜다.

* Machine instructions
* Assembler instructions
* Macro instructions

### Machine instructions
어셈블러가 컴퓨터가 실행할 수 있는 기계어 코드로 번역하기 때문에 기계 명령이라고 한다. 어셈블리 타임에 어셈블러는 기계 명령어의 어셈블러 언어 표현을 해당 개체 코드로 변환합니다. 컴퓨터는 실행 시간에 이 개체 코드를 처리합니다. 따라서 이 절에서 설명하는 함수를 execution-time functions 라고 할 수 있다.

Machine instruction 에는 5가지 종류가 있다.
* General instructions : <br>
  일반 레지스터나 저장소에 있거나 명령 스트림에서 도입된 데이터를 조작하려면 General instructions을 사용한다. General instructions 에는 fixed point, logical, and branching instructions 이 있다. 
* Decimal instructions : <br>
  binary 데이터에 대해 산술 및 편집 작업을 수행하려는 경우 Decimal instructions 를 사용한다. 10진수 데이터는 Zoned 형식 또는 Packed 형식으로 표시된다. 
* Floating-Point instructions : <br>
  부동 소수점 형식의 데이터에 대해 산술 연산을 수행하려는 경우 Floating-point instructions 사용한다. 큰 숫자와 작은 숫자 모두에 대해 산술 연산을 수행할 수 있으며 일반적으로 고정 소수점 십진 명령어보다 더 높은 정밀도를 제공한다.
* Control instructions : <br>
  입력/출력 명령어를 제외한 모든 권한 및 반 권한 기계 명령어가 포함된다. 권한 있는 명령어는 프로세서가 supervisor state 일 때만 처리된다. problem state 에서 권한 있는 명령을 처리하려는 시도는 작업 예외를 생성한다. 권한이 없는 명령은 특정 권한 요구 사항이 충족될 때 문제 상태에서 처리할 수 있는 명령이다. 권한 요구 사항이 충족되지 않을 때 문제 상태에서 권한이 없는 명령을 처리하려는 시도는 위반된 특정 요구 사항에 따라 특권 작업 예외 또는 일부 다른 프로그램 중단 조건을 생성한다.
* Input/Output operations : <br>
  입력 및 출력 작업을 보다 밀접하게 제어하려면 Input/output operations을 사용한다. Input/output operations을 사용하면 입력 또는 출력 작업이 수행될 채널 또는 장치를 식별할 수 있다.

Machine instructions 은 operation 와 operand 들로 이루어져 있다. operand 들은 register 또는 특정 메모리 영역일 수 있다. 

아래 그림 처럼 Assembly Language instructions의 길이가 1,2 또는 3 Halfword(2byte)로 나뉘고 항상 2로 균등하게 나눌 수 있는 주소인 Halfword 경계에 정렬된다. 이러한 instructions의 길이는 여러 format 에 의해 정리된다. 아래 그림은 format 별 길이이다.

![instruction_length]({{site.baseurl}}/attach/instruction_length.png)

여기서 OPCODE 는 모든 명령어의 첫 번째 필드의 길이가 1바이트인 연산코드 이고, 바이트 opcode가 있는 명령어이지만 응용 프로그램 프로그래밍에서는 일반적으로 사용되지 않는다.

format 에 대한 자세한 내용은 아래와 같다.

#### Machine Instruction Formats

* RR format instruction : <br>
RR format instruction 길이는 16비트(1 halfword) 이다. 첫 번째 바이트에는 opcode가 포함되고 두 번째 바이트에는 두 개의 피연산자가 포함되며 둘 다 레지스터이다. 아래 관련 그림이다.

![rr_instruction_format]({{site.baseurl}}/attach/rr_instruction_format.png)

RR instruction 의 한 예로 AR instruction 이 있다. 아래 예시이다.

![example_rr_instruction_format]({{site.baseurl}}/attach/example_rr_instruction_format.png)

1A 는 AR instruction 의 고유 식별자인 opcode 이며 첫번째 피연산자로 R3이 두번째 피연산자로 R4가 오고 두 레지스터가 가지고 있는 값을 더하여 R3에 저장 한다.

* RX format instruction : <br>
RX format instruction 길이는 32비트(2 halfword) 이다. 첫 번째 바이트는 opcode를 포함하며. 다음 니블은 레지스터인 첫번째 피연산자 포함한다. 두번째 피연산자는 세 가지 구성 요소가 포함한다. 아래 관련 그림이다.

  - X2 : index 레지스터
  - B2 : Base 레지스터
  - D2 : 12비트 크기의 displacement

![rx_instruction_format]({{site.baseurl}}/attach/rx_instruction_format.png)

RX instruction 의 한 예로 S instruction 이 있다. 아래 예시이다.

![example_rx_instruction_format]({{site.baseurl}}/attach/example_rx_instruction_format.png)

5b는 S instruction 의 고유 식별자인 opcode 이며 첫 번째 피연산자는 R5 이다. 두 번째 피연산자는 R12 값에 R8과 변위 값 12E 를 더한 4바이트 절대주소 위치의 값이다.
이 명령어가 실행되면 두번째 피연산자 값을 R5에서 차감하고 결과를 R5에 저장한다.

* RS format instruction : <br>
RS format instruction 길이는 32비트(2 halfword) 이다. 명령어의 첫 번째 바이트는 opcode입니다. 그 다음에는 첫 번째 및 세 번째 피연산자(둘 모두 레지스터)가 온다. 마지막으로 두 번째 피연산자인 storage 피연산자는 기본 변위 형식으로 지정된다. 아래 관련 그림이다.

![rs_instruction_format]({{site.baseurl}}/attach/rs_instruction_format.png)

RS instruction 의 한 예로 LM instruction 이 있다. 아래 예시이다.

![example_rs_instruction_format]({{site.baseurl}}/attach/example_rs_instruction_format.png)

98은 LM instruction 의 opcode입니다. 첫 번째 피연산자는 R4 이다. 세 번째 피연산자는 R9 이다. 두 번째 피연산자는 저장 위치에서 시작하는 6개의 연속적인 Full Word(4바이트) 이며, 기본 레지스터인 R12의 내용에 변위 124를 추가하여 절대 주소가 형성된다. 이 명령어가 실행되면 저장 위치의 첫 번째 FullWord R4에 R5,R6,R7,R8,R9 가 차례로 로드된다.

* SI format instruction : <br>
SI format instruction의 길이는 32비트(2 halfword)이다. 명령어의 첫 번째 바이트는 opcode 이다. 다음 바이트인 두 번째 피연산자는 값 자체를 의미하는 피연산자이다. 이 바이트는 주소가 아닌 실제 피연산자를 포함 한다. 피연산자는 정확히 1바이트 길이 이다. 나머지 바이트는 첫 번째 피연산자를 지정하는 Base 레지스터에 변위 값을 더한 주소에 저장된 값을 의미 한다. 아래 관련 그림이다.

![si_instruction_format]({{site.baseurl}}/attach/si_instruction_format.png)

SI instruction 의 한 예로 MVI instruction 이 있다. 아래 예시이다.

![example_si_instruction_format]({{site.baseurl}}/attach/example_si_instruction_format.png)

92는 MVI instruction 의 opcode 이다. space 를 의미하는 EBCDIC 코드로 0x40(ASCII 에서는 0x20 이다.)은 두 번째 피연산자이다. 첫 번째 피연산자는 Base 레지스터인 R12의 내용에 변위 33A를 추가하여 형성된 절대 저장 주소의 바이트이다. 이 명령어가 실행될 때 첫 번째 피연산자에 의해 지정된 주소로 1바이트 공백이 들어간다. 

* SS format instruction : <br>
두가지의 명령어 형식이 있다. 첫 번째 형식에는 두 피연산자 모두에 적용되는 단일 길이 사양이고, 두 번째는 각 피연산자에 대해 별도의 길이가 지정된다.

두 개의 길이가 있는 경우 각각은 니블(4bit)로 지정되며 0 – 15의 값을 포함할 수 있다. 단일 길이 사양이 있는 경우 길이가 1바이트이고 0 – 255의 값을 포함할 수 있다. 기계 명령어의 모든 길이는 실제 길이보다 1 작은 값으로 코딩된다. 0 길이는 작업에 의미가 없기 때문에 최대 기능을 제공하기 위해 기계 길이 사양 0은 실제 길이 1을 나타내고 1은 2를 나타내는 식이다. 명령어의 길이는 48비트(3 halfword)이다. 첫 번째 바이트는 opcode이고, 다음 바이트는 길이를 의미한다. 그 다음에 첫번째 피연산자와 두번째 피연산자 온다. 둘 다 Base 레지스터 + 변위 형식으로 지정된다. 아래 관련 그림이다.

![ss_instruction_format]({{site.baseurl}}/attach/ss_instruction_format.png)

SS instruction 의 한 예로 MVC instruction 이 있다. 아래 예시이다.

![example_ss_instruction_format]({{site.baseurl}}/attach/example_ss_instruction_format.png)

D2는 MVC instruction의 opcode이다. 63은 100바이트의 실제 길이에 해당하는 hex 값이다. 첫 번째 피연산자는 R12의 내용에 124의 변위를 더하여 절대 주소 메모리에 있는 100자 문자열이다. 두 번째 피연산자는 R12의 내용에 388의 변위를 추가하여 절대 주소 메모리에 있는 100자 문자열이다. 이 명령어가 실행되면 두 번째 피연산자의 100바이트가 첫 번째 피연산자로 복사되어 첫 번째 피연산자의 원래 값을 유지할 수 없다.

### Assembler instructions
Assembler instructions은 소스 모듈을 어셈블리하는 동안 특정 작업을 수행하도록 어셈블러에 요청하는 것이다. 예를 들어 데이터 상수 정의, 저장 영역 할당, 소스 모듈의 끝 정의 등이 있다. 상수를 정의하는 명령어와 alignment 위한 명령어를 생성하는 데 사용되는 명령어를 제외하고 어셈블러는 어셈블러 명령어를 object 코드로 변환 되지 않고 Machine instruction 으로 변환된다.

다음 표는 유형별 어셈블러 명령어를 나열하고 명령어이다.

<table>
  <tr>
    <td>Type of Instruction</td>
    <td>Instruction</td>
  </tr>
  <tr>
    <td rowspan="13">Program Control</td>
    <td>AINSERT</td>
  </tr>
  <tr>
    <td>CNOP</td>
  </tr>
  <tr>
    <td>COPY</td>
  </tr>
  <tr>
    <td>END</td>
  </tr>
  <tr>
    <td>EXITCTL</td>
  </tr>
  <tr>
    <td>ICTL</td>
  </tr>
  <tr>
    <td>ISEQ</td>
  </tr>
  <tr>
    <td>LTORG</td>
  </tr>
  <tr>
    <td>ORG</td>
  </tr>
  <tr>
    <td>POP</td>
  </tr>
  <tr>
    <td>PUNCH</td>
  </tr>
  <tr>
    <td>PUSH</td>
  </tr>
  <tr>
    <td>REPRO</td>
  </tr>
  <tr>
    <td rowspan="5">Listing Control</td>
    <td>CEJECT</td>
  </tr>
  <tr>
    <td>EJECT</td>
  </tr>
  <tr>
    <td>PRINT</td>
  </tr>
  <tr>
    <td>SPACE</td>
  </tr>
  <tr>
    <td>TITLE</td>
  </tr>
  <tr>
    <td rowspan="1">Operation Code Definition</td>
    <td>OPSYN</td>
  </tr>
  <tr>
    <td rowspan="15">Program Section and Linking</td>
    <td>ALIAS</td>
  </tr>
  <tr>
    <td>AMODE</td>
  </tr>
  <tr>
    <td>COM</td>
  </tr>
  <tr>
    <td>CSECT</td>
  </tr>
  <tr>
    <td>CXD</td>
  </tr>
  <tr>
    <td>DSECT</td>
  </tr>
  <tr>
    <td>DXD</td>
  </tr>
  <tr>
    <td>ENTRY</td>
  </tr>
  <tr>
    <td>EXTRN</td>
  </tr>
  <tr>
    <td>LOCTR</td>
  </tr>
  <tr>
    <td>RMODE</td>
  </tr>
  <tr>
    <td>RSECT</td>
  </tr>
  <tr>
    <td>START</td>
  </tr>
  <tr>
    <td>WXTRN</td>
  </tr>
  <tr>
    <td>XATTR</td>
  </tr>
  <tr>
    <td rowspan="2">Base Register</td>
    <td>DROP</td>
  </tr>
  <tr>
    <td>USING</td>
  </tr>
  <tr>
    <td rowspan="5">Data Definition</td>
    <td>CCW</td>
  </tr>
  <tr>
    <td>CCW0</td>
  </tr>
  <tr>
    <td>CCW1</td>
  </tr>
  <tr>
    <td>DC</td>
  </tr>
  <tr>
    <td>DS</td>
  </tr>
  <tr>
    <td rowspan="1">Symbol Definition</td>
    <td>EQU</td>
  </tr>
  <tr>
    <td rowspan="1">Associated Data</td>
    <td>ADATA</td>
  </tr>
  <tr>
    <td rowspan="2">Assembler Options</td>
    <td>PROCESS</td>
  </tr>
  <tr>
    <td>ACONTROL</td>
  </tr>
</table>

이중 몇가지 instruction 에 대해 보면 아래와 같다.

* Program Control
  * COPY
  * LTORG
  * ORG
  * END
* Program Section and Linking
  * AMODE
  * RMODE
  * CSECT
  * DSECT
* Base Register
  * DROP
  * USING
* Data Definition
  * DC
  * DS

#### Program Control
* COPY : <br>
  COPY 명령을 사용하여 소스 언어 라이브러리에서 소스 명령문을 가져와 어셈블 중인 프로그램에 포함한다. 따라서 자주 사용되는 동일한 코드 시퀀스를 반복해서 작성하는 것을 피할 수 있다.

* LTORG : <br>
  어셈블러가 literal를 수집하여 literal pool로 어셈블할 수 있도록 LTORG 명령을 사용한다. literal pool에는 이전 LTORG 명령 뒤 또는 소스 모듈 시작 후 소스 모듈에서 지정한 리터럴이 포함된다. 자세한 설명은 defining data 에서 다룬다.

* ORG : <br>
  ORG 명령은 위치 카운터의 설정을 변경하여 현재 제어 섹션의 구조를 제어한다. 이것은 컨트롤 섹션의 일부를 재정의한다. 제어 섹션이 이전에 설정되지 않은 경우 ORG는 이름 없는(private) 제어 섹션으로 시작한다.

  다음 예는 경계 및 오프셋 피연산자를 사용하는 ORG의 몇 가지 예를 보여준다.

        origin      csect
                    ds 235x               * 235바이트 크기 정의
                    org origin,,3         * 위치 카운터를 시작 지점의 + 3 으로 이동
                    org *,8               * Align on 8 byte boundary
                    org *,8,-2            * Align to 8 byte boundary -2 bytes
        translate   dc cl256’ ’           * 정렬된 translate 테이블 정의
                    org translate+c’a’
                    dc c’abcdefghi’
                    org translate+c’j’
                    dc c’jklmnopqr’
                    org translate+c’s’
                    dc c’stuvwxyz’
                    org translate+c’A’
                    dc c’ABCDEFGHI’
                    org translate+c’J’
                    dc c’JKLMNOPQR’
                    org translate+c’S’
                    dc c’STUVWXYZ’
                    org ,

    결과 적으로 translate 테이블에는 ascii 기준 알파벳이 대문자와 소문자가 정렬되어 들어간다.

* END : <br>
  END 명령어를 사용하여 프로그램 어셈블리를 종료한다. 주소를 입력할 수도 있다. 프로그램이 로드된 후 제어가 전송될 수 있는 피연산자 필드이다. END 명령어는 항상 소스 프로그램의 마지막 명령문이어야 한다.    

#### Program Section and Linking
* AMODE : <br>
  AMODE 명령어는 제어 섹션, ENTRY 기호 또는 EXTRN에 대한 주소 지정 모드를 지정한다. 아래 AMODE 에 대한 format 이다.
  
    ![amode_railload]({{site.baseurl}}/attach/amode_railload.png)

* RMODE : <br>
  RMODE 명령어는 제어 세션 연관된 프로그램을 로드할때 주소 지정 모드를 지정한다. 아래 RMODE 에 대한 format 이다.

    ![rmode_railload]({{site.baseurl}}/attach/rmode_railload.png)

  - AMODE, RMODE 는 어셈블리의 아무 곳에서나 지정할 수 있다. 명명되지 않은 제어 섹션에서는 적용되지 않는다.
     
  - 어셈블리에는 여러 AMODE, RMODE 명령어가 있을 수 있다. 그러나 두 개의 AMODE, RMODE 명령어는 동일한 이름 필드를 가질 수 없다.

  - AMODE와 RMODE의 유효한 조합과 무효한 조합은 다음 표에 나와 있다.

    |                | RMODE 24 | RMODE 31 | RMODE 64 |
    |----------------|----------|----------|----------|
    |AMODE 24        |    OK    |  invalid | invalid  |
    |AMODE 31        |    OK    |    OK    | invalid  |
    |AMODE ANY/ANY31 |    OK    |    OK    | invalid  |
    |AMODE 64/ANY64  |    OK    |    OK    |    OK    |

  - 모드가 없거나 하나의 MODE가 지정된 경우 사용되는 기본값은 다음 표와 같다.

    |  Specified           |   Default                |
    |----------------------|--------------------------|
    | Neither              | AMODE 24, RMODE 24       |
    | AMODE 24             | RMODE 24                 |
    | AMODE 31             | RMODE 24                 |
    | AMODE ANY / ANY 31   | RMODE 24                 |
    | RMODE 24             | AMODE 24                 |
    | RMODE 31             | AMODE 31                 |
    | AMODE 64             | RMODE 31                 |
    | RMODE ANY 64         | RMODE 31                 |
    | RMODE 64             | AMODE 64                 |

* CSECT : <br> 
  CSECT 명령어는 실행 가능한 제어 섹션이거나 실행 가능한 제어 섹션의 연속을 나타낸다. 하나의 어셈블리 코드 내에 CSECT 가 여러개 올수 있고 이는 entry 가 여러개 있다고 생각해도 좋다. 아래 CSECT 에 대한 format 이다.

  ![csect_railload]({{site.baseurl}}/attach/csect_railload.png)

  1. CSECT 명령어는 소스 모듈의 어느 곳에서나 사용할 수 있다. 첫 번째 실행 가능한 제어 섹션을 시작하는 데 사용되는 경우 위치 카운터에 영향을 미치므로 제어 섹션이 시작되도록 하는 명령이 선행되어서는 안된다.
  2. CSECT 의 name 필드는 제어 섹션을 식별한다. 소스 모듈 내의 여러 CSECT 명령어가 name 필드에 동일한 기호를 가지고 있는 경우 첫 번째 항목이 제어 섹션을 시작하고 나머지는 제어 섹션의 연속을 나타낸다. symbol로 표시되는 일반 심볼은 제어 섹션의 첫 번째 바이트 주소를 나타내며 길이 속성 값이 1이다.
  3. symbol 이 지정되지 않았거나 시퀀스 symbol 인 경우 CSECT 명령이 시작되거나 명명되지 않은 제어 섹션의 연속을 나타낸다. 첫 번째 제어 섹션이 START 명령에 의해 시작된 경우 섹션을 계속하는 CSECT 명령은 START 명령과 동일한 이름을 가져야 한다.
   
* DSECT : <br>
  더미 제어 섹션의 시작 또는 연속을 식별한다. 소스 모듈에서 하나 이상의 더미 섹션을 정의할 수 있다. 아래 DSECT 에 대한 format 이다.

  ![dsect_railload]({{site.baseurl}}/attach/dsect_railload.png)

  1. symbol이 일반 symbol를 나타내는 경우 일반 symbol는 더미 섹션을 식별한다. 소스 모듈 내의 여러 DSECT 명령어의 이름 필드에 동일한 symbol이 있는 경우 첫 번째 항목은 더미 섹션을 시작하고 나머지는 더미 섹션의 연속을 나타낸다. symbol로 표시된 일반 symbol는 더미 섹션의 첫 번째 바이트 주소를 나타내며 길이 속성 값이 1이다. 
  2. symbol이 지정되지 않거나 이름이 시퀀스 symbol인 경우 DSECT 명령은 이름 없는 컨트롤 섹션의 연속으로 취급된다.
  3.  더미 섹션에 대한 위치 카운터는 항상 초기 값 0으로 설정된다. 그러나 중단된 더미 제어 섹션이 DSECT 명령을 사용하여 계속되면 해당 제어 섹션에서 마지막으로 지정된 위치 카운터가 계속된다.
  4.  DSECT 명령어 뒤에 오는 소스 문은 해당 DSECT 명령어로 식별되는 더미 섹션에 속한다.
  5.  자세한 내용은 defining data 에서 다룬다.

#### Base Register
* DROP : <br>
  DROP 명령어는 USING 명령어의 도메인을 종료한다.
    - 다른 프로그래밍 목적으로 USING 명령어에 의해 이전에 할당된 기본 레지스터를 해제한다.
    - 예를 들어 두 USING 범위가 겹치거나 일치하는 경우와 같이 특정 코딩 상황에서 어셈블러가 원하는 기본 레지스터를 사용하기 다른 레지스터를 해제하는 경우가 있다.

* USING : <br>
  USING 명령어는 베이스를 지정한다. 주소와 범위를 지정하고 하나 이상의 기본 레지스터를 할당한다. 기본 주소와 함께 기본 레지스터를 로드하면 제어 섹션내 symbol에 대한 주소 접근이 가능해진다. 이를 adressability 라고 한다. 제어 섹션이 설정되지 않은 경우 USING은 명명되지 않은 제어 섹션으로 적용된다.

  USING 명령어를 올바르게 사용하려면 다음을 알아야 한다.
  * USING 명령어에 의해 제어 섹션의 어떤 위치 부터 adressability 하게 만드는지
  * 소스 모듈에서 명령어 피연산자가 symbol 일 경우 USING 에 의해 해당 symbol 이 adressability 한지

#### Data Definition
* DC : <br>
  DC 명령어를 사용하여 프로그램 실행에 필요한 데이터 상수를 정의한다. DC 명령어는 어셈블러가 어셈블된 소스 모듈의 특정 위치에 지정하는 데이터 상수의 이진 표현을 생성하도록 한다. 이 작업은 어셈블리 타임에 이루어진다. 아래 DC 에 대한 format 이다.

  ![dc_railload]({{site.baseurl}}/attach/dc_railload.png)

  DC는 단순히 프로그램 영역에 초기 데이터를 생성한다. 해당 값은 프로그램 실행 중에 수정될 수 있다. 그래서 런타임 중에 값을 보장할 수 없습니다. 값을 보장하는 상수처럼 작동하는 값을 선언하려면 리터럴을 사용해야 한다. 

  DC 명령어는 다음 유형의 상수를 생성할 수 있다.
  
  ![dc_type_table]({{site.baseurl}}/attach/dc_type_table.png)

* DS : <br>
  * 저장 공간 확보
  * 이 영역에 대한 레이블을 제공.
  * 레이블로 정의된 symbol를 참조하여 이러한 영역을 사용.
  * 제어 섹션이 이전에 설정되지 않은 경우 DS는 이름 없는 제어 섹션에 있는 것으로 적용.
  ![ds_railload]({{site.baseurl}}/attach/ds_railload.png)

  DS,DC의 Operand 는 6개의 하위 필드로 구성된 피연산자이다. 처음 5개의 하위 필드는 symbol의 속성을 설명한다. 여섯 번째 하위 필드는 암시적 길이를 결정하는 명목 값을 제공한다. 형태는 아래와 같다.
  ![ds_dc_operand_railload]({{site.baseurl}}/attach/ds_dc_operand_railload.png)

  DC 명령어와 달리 DS 명령어는 데이터를 어셈블하지 않는다. 따라서 DS 명령어 피연산자의 nominal value(6번째 서브필드, 초기값을 의미)을 지정할 필요가 없다. DS 명령어는 작업 영역, 입력 및 출력 버퍼 등에 대한 저장소를 기호적으로 정의하는 가장 좋은 방법이다.
  nominal value 서브필드는 DS 피연산자에서는 선택 사항이지만 DC 피연산자에서는 필수이다. 문자(C) 및 16진수(X) 유형 영역에 지정할 수 있는 최대 길이는 동일한 DC 피연산자의 경우 256바이트 이나 DS의 경우 65,535바이트이다. DBCS 문자 최대 길이(G) 유형은 65534바이트 이다. 
  
  Lengh modifier(L) 이 명시적으로 기재되어 있는 경우 길이값이 곧 바이트 사이즈가 되고 명시적으로 기술되어 있지 않는 경우 데이터 타입의 기본 바이트 크기가 된다. duplication factor 는 기본갑이 1 이며 명시적으로 기술되어 있는 경우 뒤에 오는 유형의 반복여부를 결정한다. 배열이라 생각하면 편하다.

### Macro instructions
매크로 명령어는 매크로 정의라고 하는 사전에 정의된 명령어 시퀀스를 처리하도록 어셈블러 프로그램에 대한 요청이다. 어셈블러는 매크로 명령어 처리에 의해 조건에 맞는 기계 및 어셈블러 명령어를 생성하여 어셈블리 소스의 일부분을 구성한다. IBM은 필요한 매크로 명령을 코딩하여 처리를 위해 호출할 수 있는 입력/출력, 데이터 관리 및 감독자 작업에 대한 매크로 정의를 제공한다. (IBM 제공하는 기본적인 매크로가 있다. 이를 38_macro라 한다.) 또한 고유한 매크로 정의를 준비하고 해당 매크로 명령어를 코딩하여 이를 호출할 수 있다. 이를 유저 매크로라 한다. 필요할 때마다 이 모든 시퀀스를 코딩하는 대신 시퀀스를 나타내는 매크로 명령을 만든 다음 시퀀스가 ​​필요할 때마다 호출한다. 이는 코딩을 위한 비용을 줄일 수 있다.

매크로는 "MACRO" header statement 로 시작해야하며, "MEND" trailer statement 로 끝나야한다.
매크로 명령어는 아래와 같은 statement 로 구성된다.
* Model statements
* Processing statements
* Comment statements
* Symbolic parameters
* System variable symbols

#### Model statements
Model statements는 조건처리에 의해 어셈블리 중에 어셈블러 언어가 생성되는 statement 이다. 생성할 statement의 형식을 결정할 수 있다. Model statements에서 variable symbols를 이용하여 다양한 형태의 구문을 대체 생성할 수 있다. Model statements은 열 1 - 71에서 하나 이상의 공백으로 구분된 하나 이상의 필드로 구성된다. 필드는 name, operator, operand 및 remark 필드라고 한다.

* Variable symbols as points of substitution : <br>
  Model statements의 name, operator, operand 필드에 나타나는 Variable symbols를 값으로 대체할 수 있다. Variable symbols의 세 가지 주요 유형은 다음과 같다.

  * Symbolic parameters(위치 또는 키워드)
  * System variable symbols
  * SET symbols(전역 범위 또는 로컬 범위 SETA, SETB 또는 SETC symbols))

예를들어 아래와 같이 사용한다.

      &PARAM(3)
      &SYSLIST(1,3)
      &SYSLIST(2)
      &SETA(10)
      &SETC(15)

##### Rules for concatenation
Model statements의 symbolic parameter 바로 앞이나 뒤에 다른 문자 또는 다른 symbolic parameter가 있는 경우 symbolic parameter에 해당하는 문자는 생성된 명령문에서 다른 문자 또는 다른 기호에 해당하는 문자와 결합된다. 이 과정을 concatenation이라고 한다.

  Variable symbols를 일반 문자열에 연결하는 경우 연결 문자(.)의 사용에는 다음 규칙이 적용된다.
    
    ![rule_concat]({{site.baseurl}}/attach/rule_concat.png)

  1. 영숫자는 변수 기호 다음에 온다.
  2. 아래 첨자를 사용하지 않는 왼쪽 괄호는 변수 기호 뒤에 온다.
  3. .. 이 두개 연속되는 경우 . 를 있는 그래도 사용하기 위함이다.
  4. 4번의 결과이다.
  5. 일반 문자열이 변수 기호 앞에 온다.
  6. 왼쪽 괄호나 마침표를 제외한 특수 문자는 변수 기호 뒤에 온다.
  7. 변수 기호는 다른 변수 기호 다음에 온다.
  8. 변수 기호는 아래 첨자와 함께 사용된다. 변수 기호와 아래 첨자 사이에 연결 문자를 사용하면 안 된다. 그렇지 않으면 문자는 연결된 문자열로 간주되며 첨자 변수 기호가 아니다.

##### Rules for model statement fields
Model statement에서 지정할 수 있는 필드는 일반 어셈블러 언어 문에서 지정할 수 있는 필드와 동일한다. name, operation, operand, and remarks 필드이다. 유효한 어셈블러 언어 명령어를 생성하려면 Model statement에는 operation 필드에 항목이 있어야 하고 대부분의 경우 operand 필드에 항목이 있어야 한다.

* Name field <br>
생성 전에 model statement의 name 필드에 허용되는 항목은 다음과 같다 : <br>
  * Space
  * ordinary symbol
  * sequence symbol
  * variable symbol
  * variable symbol 또는 &SYSNDX와 같은 system variable symbols 및 함께 연결된 기타 문자열의 조합

  생성된 결과는 공백(유효한 경우) 또는 유효한 어셈블러 또는 기계 명령어 Name 필드를 나타내는 문자열이어야 한다. 더블바이트 데이터는 어셈블러 또는 기계 명령어 Name 필드에서 유효하지 않으며 생성되어서는 안 된다. 주석 문 표시기(* 또는 .*)를 생성하는 데 변수 기호를 사용해서는 안 된다.

* Operation field <br>
생성 전에 model statement의 Operation 필드에 허용되는 항목은 다음 목록에 나와 있다 : <br>
  * 다음에 대한 작업 코드를 나타내는 일반 기호:
    * 모든 기계 명령
    * 매크로 명령어
    * MNOTE 명령어
    * variable symbol
    * 함께 연결된 변수 문자열의 조합
  * 모델 문의 작업 필드에는 다음 규칙이 적용 : <br>
    * MACRO 및 MEND 문은 model statement 에서 허용되지 않는다. 매크로 정의를 구분하는 데만 사용된다.
    * variable symbol 는 단독으로 사용하거나 연결된 문자열의 일부로 사용하여 다음에 대한 조작 코드를 생성할 수 있다.
      * 모든 기계 명령
      * LCLx, GBLx, SETx, AIF 및 AGO와 같은 조건부 어셈블리 명령어
      * 다음 어셈블러 명령어: COPY, ICTL, ISEQ, MACRO, MEND, MEXIT 및 REPRO
    * 2바이트 데이터는 조작 필드에서 유효하지 않는다.
  
* Operand field <br>
생성 전에 model statement의 Operand 필드에 허용되는 항목은 다음과 같다 : <br>
  * Space
  * ordinary symbol
  * 영숫자와 특수 문자를 결합한 문자열(variable symbol 제외)
  * variable symbol
  * 함께 연결된 variable symbol 및 기타 문자열의 조합
  * DBCS 어셈블러 옵션이 지정되면 ''' 묶인 문자열에 2바이트 데이터가 포함될 수 있다.

  허용 가능한 생성 결과는 Space(유효한 경우)와 유효한 어셈블러, 기계 명령어 또는 매크로 명령어 Operand 필드를 나타내는 문자열이다.

  variable symbol: variable symbol는 ICTL 또는 ISEQ 명령어의 Operand 필드에 사용하면 안 된다. 매크로 정의 내부에 있는 COPY 명령어의 Operand 필드에는 variable symbol를 사용해서는 안 된다.

#### Symbolic parameters
Symbolic parameter를 사용하면 호출하는 매크로 명령에서 매크로 정의의 본문으로 값을 받을 수 있다. 매크로 프로토타입 문에서 이러한 매개변수를 선언한다. 매크로 정의의 본문에서 값을 대체하는 역할을 할 수 있으며 호출 매크로 명령에 의해 할당된 값으로 대체된다.
의미 있는 이름을 가진 Symbolic parameter를 사용하여 매개변수(또는 대체된 값)가 사용되는 목적을 나타낼 수 있다.
Symbolic parameter는 유효한 변수 기호여야 한다. Symbolic parameter는 & 다음에 영문자와 0~61자의 영숫자로 구성된다.

유효한 기호 매개변수는 다음과 같다 :

    &READER &LOOP2
    &A23456 &N
    &X4F2 &$4

다음은 잘못된 기호 매개변수 이다 :

    CARDAREA  -> 첫 번째 문자는 & 이여야 한다. 
    &256B -> & 뒤의 첫 번째 문자는 알파벳이어야 한다.
    &BCD%34 -> 초기 & 이외의 특수 문자를 포함하고 있다.
    &IN AREA -> 초기 & 이외의 특수 문자 [공백]을 포함하고 있다.

Symbolic parameter에는 local scope가 있다. 즉, 할당된 이름과 값은 선언된 매크로 정의에만 적용된다.
매개변수의 값은 해당 정의의 각 호출 동안 포함하는 매크로 정의를 처리하는 동안 일정하게 유지된다. symbolic parameters 에는 아래 두가지 타입이 있다.

* Positional parameters <br>
매크로 정의를 호출할 때마다 매개변수 값을 변경하려면 매크로 정의에서 위치 매개변수를 사용하면 된다. 이는 Keyword parameter 보다 Positional parameter 에 대한 값을 제공하는 것이 더 쉽기 때문이다. 호출하는 매크로 명령어의 피연산자에서 해당 인수의 위치에 원하는 값만 쓰면 된다. 그러나 많은 매개변수가 필요한 경우 Keyword parameter 를 시용하는 것이 좋다. 키워드를 사용하면 값이 지정된 매개변수를 알려줌으로써 각 호출에서 지정해야 하는 개별 값을 쉽게 추적할 수 있다. 아래 관련 그림이다.

![positional_parameters]({{site.baseurl}}/attach/positional_parameters.png)

* Keyword parameters <br>
자주 변경되지 않는 값에 대해 매크로 정의에서 Keyword parameter를 사용하거나 매개변수가 많은 경우 Keyword parameter를 사용하는 것이 좋다. 피연산자에서 반복되는 키워드는 어떤 매개변수에 값이 주어지고 어떤 목적으로 매개변수가 사용되고 있는지 알기 쉽다. Keyword parameter 에 할당할 표준 기본값을 지정하면 호출 매크로 명령어에서 해당 키워드 인수 피연산자를 생략할 수 있따. 호출 매크로 명령어에서 해당 키워드 피연산자를 임의의 순서로 지정할 수 있다. 아래 관련 그림이다.

![keyword_parameters]({{site.baseurl}}/attach/keyword_parameters.png)

#### Processing statements
이 섹션에서는 이러한 Processing statements에 대한 정보를 제공한다.

* Conditional assembly instructions <br>
Conditional assembly instructions를 사용하면 생성된 문의 내용과 생성 순서를 Conditional assembly 시간에 결정할 수 있다. 다음은 지침과 해당 기능이다.

| Conditional Assembly             | Operation Done |
|----------------------------------|----------------|
| GBLA, GBLB, GBLCLCLA, LCLB, LCLC | variable symbols 선언(전역 범위 및 지역 범위 SET symbols) 및 기본 초기값 설정 |
| SETA, SETB, SETC | variable symbols에 값 할당 (SET symbols) |
| SETAF, SETCF | 외부 variable symbols에 대한 값 할당 (SET symbols) |
| ACTR | Loop 카운터 설정 |
| AGO | 무조건 분기 |
| AIF | 조건부 분기 |
| ANOP | 다음 순차 명령어로 제어 전달 |

* Inner macro instructions <br>
매크로 명령어는 매크로 정의 내에 중첩될 수 있으므로 사용자 정의 내에서 다른 매크로를 호출할 수 있다.

* Other conditional assembly instructions <br>
매크로 정의를 작성하는 데 도움이 될 수 있는 몇 가지 추가 지침이 있다. 다음은 지침과 해당 기능이다 : <br>

| Inner Macro Instruction | Operation Done |
| ----------------------- | -------------- |
| AEJECT | 다음 statement 로 진행 |
| AINSERT | 입력 스트림에 statement  삽입 |
| AREAD | variable symbol에 임의의 문자열 할당 (SETC symbol) |
| ASPACE | 목록에 하나 이상의 빈 줄 삽입 | 
| COPY | 라이브러리으로 소스 복사 |
| MEXIT | 매크로 정의 종료 |

#### Comment statements
* Ordinary comment statements
일반 주석 문을 사용하면 매크로 정의에서 생성된 출력에 대해 설명할 수 있다. 일반 주석 문은 매크로 정의 및 공개 코드에서 사용할 수 있다. 일반 주석 문은 시작 열의 * 와 그 뒤에 오는 문자열로 구성된다. 주석 문은 어셈블러가 어셈블러 문을 생성하기 위해 다른 Model statement을 사용하는 것처럼 어셈블러에서 어셈블러 언어 주석 문을 생성하는 데 사용된다. variable symbol 대체가 수행되지 않는다.

* Internal macro comment statements
매크로가 처리될 때  conditional assembly 동안 수행되는 작업을 설명하기 위해 매크로 정의 본문에 내부 매크로 주석을 작성할 수도 있다. 내부 매크로 주석 문은 시작 열의 마침표, *, 문자열로 구성됩니다. 내부 매크로 주석 문에 지정된 변수 기호는 값으로 대체되지 않는다. 내부 매크로 주석 문은 매크로 정의의 아무 곳에나 나타날 수 있다.

#### System variable symbols
시스템 변수 기호는 &SYS 문자로 시작하는 변수 기호의 특수 클래스이다. 값은 특정 규칙에 따라 어셈블러에 의해 설정된다. 로컬 범위 SET 기호 또는 전역 범위 SET 기호에서 선언하거나 매크로 프로토타입 문에서 symbolic parameters로 사용할 수 없다. 이러한 기호를 Model statement 및 conditional assembly instructions 에서 대체 point로 사용할 수 있다. 

모든 시스템 variable symbols에는 다른 variable symbols와 동일한 연결 및 대체 규칙이 적용된다. 
자세한 시스템 변수에 대해서는 다음 url 를 참조한다 :
[ibm System variable symbols](https://www.ibm.com/docs/en/hla-and-tf/1.6?topic=definitions-system-variable-symbols)