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
어셈블러(Assembler)는 어셈블러 언어, 혹은 어셈블리 언어(Assembler language 혹은 Assembly language)로 작성된 소스 코드를 컴퓨터가 이해할 수 있는 기계어로 변환하는 작업을 수행한다.
어셈블러 언어는 형식과 내용 면에서 기계어에 가장 가까운 기호 프로그래밍 언어이다. 어셈블러 언어는 다음과 같은 경우에 유용하다.

* 바이트 및 비트 수준까지 프로그램을 밀접하게 제어해야 하는 경우
* COBOL, Fortran 또는 PL/I와 같은 다른 프로그래밍 언어에서 제공하지 않는 기능에 대한 서브루틴을 작성해야 하는 경우 

어셈블러 언어는 명령이나 주석을 나타내는 명령문으로 구성된다. 명령어(Instruction)는 언어의 작동 부분이며 다음 세 그룹으로 나뉜다.

* 기계 명령어(Machine instructions)
* 어셈블러 명령어(Assembler instructions)
* 매크로 명령어(Macro instructions)

### Machine instructions
어셈블러는 기계 명령어를 컴퓨터가 실행할 수 있는 기계어 코드로 번역한다. 하나의 기계 명령어는 오직 하나의 기계어 코드와 매칭된다. 어셈블리 과정에서 어셈블러는 기계 명령어의 어셈블러 언어 표현을 목적코드(Object code)로 변환한다. 컴퓨터는 실행 시간에 이 목적코드를 처리한다.

기계 명령어에는 5가지 종류가 있다.
* 범용 명령어(General instructions) : <br>
  범용 레지스터(General Purpose Register: GPR)나 저장소에 있거나, 명령 스트림에서 입력된 데이터를 조작하려면 범용 명령어를 사용한다. 범용 명령어에는 고정소수점(fixed point), 논리(logical), 브랜치(branching) 명령어 등이 존재한다.
* 십진수 명령어(Decimal instructions) : <br>
  이진(Binary) 데이터에 대해 산술 및 편집 작업을 수행하려는 경우 십진수 명령어를 사용한다. 10진수 데이터는 Zoned 형식 또는 Packed 형식으로 표시된다. 
* 부동소수점 명령어(Floating-Point instructions) : <br>
  부동소수점 형식의 데이터에 대해 산술 연산을 수행하려는 경우 부동소수점 명령어를 사용한다. 큰 숫자와 작은 숫자 모두에 대해 산술 연산을 수행할 수 있으며 일반적으로 고정 소수점 및 십진수 명령어보다 더 높은 정밀도를 제공한다.
* 제어 명령어(Control instructions) : <br>
  입력/출력 명령어를 제외한 모든 권한 및 반-권한 기계 명령어가 포함된다. 권한 있는 명령어는 프로세서가 Supervisor state 일 때만 처리된다. Problem state 에서 권한이 필요한 명령을 처리하려는 시도는 예외를 발생시킨다. 권한이 없는 명령은 특정 권한 요구 사항이 충족될 때 Problem state에서 처리할 수 있는 명령이다. 권한 요구 사항이 충족되지 않을 때 Problem state에서 권한이 없는 명령을 처리하려는 시도는 위반된 특정 요구 사항에 따라 특권 작업 예외 또는 일부 다른 프로그램의 중단 조건을 발생시킨다.
* 입출력 연산(Input/Output operations) : <br>
  입력 및 출력 작업을 보다 밀접하게 제어하려면 입출력 연산을 사용한다. 입출력 연산을 사용하면 입력 또는 출력 작업이 수행될 채널 또는 장치를 식별할 수 있다.

기계 명령어는 연산자(Operation)와 피연산자들(Operands)로 이루어져 있다. 피연산자는 레지스터 또는 특정 메모리 영역일 수 있다.

아래 그림처럼 목적코드에서 기계 명령어가 차지하는 길이는 1,2 또는 3 Halfword 로 나뉘고 항상 2로 균등하게 나눌 수 있는 주소인 Halfword 경계에 정렬(Halfword-align)된다. 명령어 길이는 각 포맷에 따라 달라진다. 아래 그림은 명령어 포맷 별 길이를 나타내었다.

![instruction_length]({{site.baseurl}}/attach/instruction_length.png)

모든 기계 명령어는 첫 1바이트에 1바이트 길이를 가지는 연산자 코드(OPCODE: Operation code)를 가지고 있다(2바이트 길이를 가지는 OPCODE도 있지만, 일반적인 어플리케이션 프로그래밍에서는 잘 사용하지 않는다). 각 기계 명령어는 고유의 OPCODE를 가지고 있으며 덧셈 연산, 뺄셈 연산, 데이터 이동 혹은 비교 연산 등 각 명령어가 어떤 동작을 할 것인지 결정짓는다.

#### Machine Instruction Formats

##### RR 포맷 명령어
RR 포맷 명령어의 길이는 16비트(1 halfword)이다. 첫 번째 바이트는 OPCODE를 나타내며, 두 번째 바이트에는 두 개의 피연산자를 각각 4비트(1니블)로 표현하며, 이것들은 모두 레지스터를 나타낸다.

![rr_instruction_format]({{site.baseurl}}/attach/rr_instruction_format.png)

RR 포맷 명령어의 한 예로 AR 명령어가 있다. 아래는 `AR  R3,R4` 구문을 통해, AR 명령어가 동작하는 방식을 간단히 나타낸 그림이다.

![example_rr_instruction_format]({{site.baseurl}}/attach/example_rr_instruction_format.png)

위의 예제에서 1A 는 AR 명령어의 OPCODE이며 첫 번째 피연산자는 R3, 두 번째 피연산자는 R4이다. 따라서 위의 예제 구문은 **R3에 저장된 값**과 **R4에 저장된 값**을 서로 더하여 **R3에 저장**하는 동작을 수행한다.

##### RX 포맷 명령어
RX 포맷 명령어의 길이는 32비트(2 halfword)이다. 첫 번째 바이트는 OPCODE를 나타내며, 다음 니블은 레지스터인 첫 번째 피연산자를 나타낸다. 두 번째 피연산자는 20비트를 차지하며, 아래 세 가지 구성 요소를 포함한다.

  - X2 : 인덱스 레지스터(Index register), 4비트
  - B2 : 베이스 레지스터(Base register), 4비트
  - D2 : 12비트 크기의 변위값(Displacement), 12비트

아래는 이러한 내용을 나타낸 그림이다.

![rx_instruction_format]({{site.baseurl}}/attach/rx_instruction_format.png)

RX 포맷 명령어의 한 예로 S 명령어가 있다. 아래는 `S  R5,302(R8,R12)`구문을 통해, S 명령어가 동작하는 방식을 간단히 나타낸 그림이다.

![example_rx_instruction_format]({{site.baseurl}}/attach/example_rx_instruction_format.png)

위의 예제에서 5B는 S 명령어의 OPCODE 이며 첫 번째 피연산자는 R5, 두 번째 피연산자는 R12 값에 R8과 변위값 302(0x12E)를 더한 4바이트 절대 주소 위치의 값이다.
따라서 위의 예제 구문은 **R5에 저장된 값**에서 **R12에 저장된 주소 + R8에 저장된 주소 + 302(0x12E)를 더한 결과 주소에 저장된 값**을 빼서 **R5에 저장**하는 동작을 수행한다.

##### RS 포맷 명령어
RS 포맷 명령어의 길이는 32비트(2 halfword)이다. 첫 번째 바이트는 OPCODE를 나타내며, 그 다음 바이트에는 첫 번째 및 세 번째 피연산자(둘 모두 레지스터)를 각각 4비트로 표현한다. 두 번째 피연산자는 베이스 레지스터 + 변위값(Base register + Displacement) 형식으로 표현한다. 아래 관련 그림이다.

![rs_instruction_format]({{site.baseurl}}/attach/rs_instruction_format.png)

RS 포맷 명령어의 한 예로 LM 명령어가 있다. 아래는 `LM  R4,R9,292(R12)` 구문을 통해, RS 명령어가 동작하는 방식을 간단히 나타낸 그림이다.

![example_rs_instruction_format]({{site.baseurl}}/attach/example_rs_instruction_format.png)

98은 LM 명령어의 OPCODE 이다. 첫 번째 피연산자는 R4, 세 번째 피연산자는 R9이다(순서상 두 번째 위치에 오지만, 첨자로 붙은 숫자는 3임에 유의). 두 번째 피연산자는 지정한 위치에서 시작하는 6개의 연속적인 Full word(4바이트)이며, 베이스 레지스터인 R12의 내용에 변위 0x124를 추가하여 절대 주소가 형성된다. 이 명령어가 실행되면 저장 위치의 첫 번째 FullWord R4에 R5,R6,R7,R8,R9 가 차례로 로드된다.

##### SI 포맷 명령어
SI 포맷 명령어의 길이는 32비트(2 halfword)이다. 첫 번째 바이트는 OPCODE를 나타내며, 다음 바이트인 두 번째 피연산자는 값 자체를 의미하는 피연산자이다. 이 바이트는 주소가 아닌 실제 피연산자 대상 데이터를 그대로 표현한다. 피연산자는 정확히 1바이트 길이이다. 나머지 바이트는 첫 번째 피연산자인 베이스 레지스터 + 변위값 형식으로 표현된 주소를 의미 한다. 두 번째 피연산자 데이터는 첫 번째 피연산자가 가리키는 저장소에 저장된다. 아래 관련 그림이다.

![si_instruction_format]({{site.baseurl}}/attach/si_instruction_format.png)

SI 포맷 명령어의 한 예로 MVI 명령어가 있다. 아래는 `MVI 826(12),' '` 구문을 통해, SI 명령어가 동작하는 방식을 간단히 나타낸 그림이다.

![example_si_instruction_format]({{site.baseurl}}/attach/example_si_instruction_format.png)

92는 MVI 명령어 의 OPCODE 이다. 공백을 의미하는 EBCDIC 코드로 0x40(ASCII 에서는 0x20 이다.)은 두 번째 피연산자이다. 첫 번째 피연산자는 베이스 레지스터인 R12의 내용에 변위값 826(0x33A)를 추가하여 형성된 저장소의 주소를 나타낸다. 이 명령어가 실행되면 첫 번째 피연산자에 의해 지정된 주소로 1바이트 공백값이 들어간다.

##### SS 포맷 명령어
다른 명령어들과 달리, SS 포맷 명령어는 두 가지의 명령어 형식이 있다. 첫 번째 형식은 두 피연산자 모두에 적용되는 단일 길이를 가지는 것이며, 두 번째 형식은 각 피연산자에 대해 별도의 길이가 각각 지정되는 것이다.

단일 길이 사양의 경우, 길이가 1바이트이고 0 – 255의 값을 표현할 수 있다. 두 개의 길이가 따로 존재하는 경우, 각 길이는 니블(4bit)로 표현되며 각각 0 – 15의 값을 표현할 수 있다. 기계 명령어의 모든 길이값은 실제 길이보다 1 작은 값으로 코딩된다. 0 길이는 작업에 의미가 없기 때문에 최대 기능을 제공하기 위해 기계 길이 사양 0은 실제 길이 1을 나타내고 1은 2를 나타내는 식이다. 명령어의 길이는 48비트(3 halfword)이다. 첫 번째 바이트는 OPCODE이고, 다음 바이트는 길이를 의미한다. 그 다음에 첫 번째 피연산자와 두 번째 피연산자가 차례로 위치한다. 두 피연산자 모두 베이스 레지스터 + 변위값 형식으로 표현한다. 아래 관련 그림이다.

![ss_instruction_format]({{site.baseurl}}/attach/ss_instruction_format.png)

SS 포맷 명령어의 한 예로 MVC 명령어가 있다. 아래는 `MVC 292(100,R12),904(R12)` 구문을 통해, SS 명령어가 동작하는 방식을 간단히 나타낸 그림이다.

![example_ss_instruction_format]({{site.baseurl}}/attach/example_ss_instruction_format.png)

위의 예시에서 0xD2는 MVC 명령어의 OPCODE이다. 첫 번째 피연산자는 R12의 내용에 292(0x124)의 변위를 더하여 얻을 수 있는 메모리 주소에 위치한 100(0x63)바이트짜리 문자열을 가리킨다. 두 번째 피연산자는 R12의 내용에 904(0x388)의 변위를 더하여 얻을 수 있는 메모리 주소에 위치한 100바이트짜리 문자열을 가리킨다. 이 명령어가 실행되면 두 번째 피연산자가 가리키는 100바이트 문자열이 첫 번째 피연산자가 가리키는 주소 공간으로 복사된다.

### Assembler instructions
어셈블러 명령어(Assembler instructions)는 소스 모듈을 어셈블리하는 동안 특정 작업을 수행하도록 어셈블러에 요청하는 것이다. 예를 들어 데이터 상수 정의, 저장 영역 할당, 소스 모듈의 시작과 끝을 정의하는 등의 기능을 수행한다. 어셈블러는 실제 기계 명령어로 변환되는 일부 명령어(상수를 정의하는 명령어와 바이트 정렬을 위해 사용되는 명령어 등)를 제외한 어셈블러 명령어를 목적코드로 변환하지 않는다.

다음은 유형별로 어셈블러 명령어를 나열한 표이다.

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

이중 일부 명령어에 대해 아래와 같이 분류할 수 있다.

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
  COPY 명령을 사용하여 소스 언어 라이브러리에서 소스 명령문을 가져와 컴파일 중인 프로그램에 포함시킨다. 따라서 자주 사용되는 동일한 코드 시퀀스를 반복해서 작성하는 것을 피할 수 있다.
  (OFASM의 경우, LD_LIBRARY_PATH에 위치한 소스를 가져와 프로그램에 포함시킨다.)

* LTORG : <br>
  어셈블러가 리터럴를 수집하여 리터럴풀(literal pool)을 생성 할 수 있는 기능을 제공한다. 리터럴풀에는 이전 LTORG 명령 뒤 또는 소스 모듈 시작 후 소스 모듈에서 지정한 리터럴이 포함된다. 자세한 설명은 defining data 에서 다룬다.

* ORG : <br>
  ORG 명령은 로케이션 카운터의 설정을 변경하여 현재 제어 섹션(Control section)의 구조를 다룰 수 있게 한다. 이것은 제어 섹션의 일부를 재정의한다. 제어 섹션이 이전에 설정되지 않은 경우 ORG는 이름 없는(private) 제어 섹션으로 시작한다.

  다음 예는 경계 및 오프셋 피연산자를 사용하는 ORG의 몇 가지 예를 보여준다.

        ORIGIN      CSECT
                    DS 235X               * 235바이트 크기 정의
                    ORG ORIGIN,,3         * 위치 카운터를 시작 지점의 + 3 으로 이동
                    ORG *,8               * ALIGN ON 8 BYTE BOUNDARY
                    ORG *,8,-2            * ALIGN TO 8 BYTE BOUNDARY -2 BYTES
        TRANSLATE   DC CL256’ ’           * 정렬된 TRANSLATE 테이블 정의
                    ORG TRANSLATE+C’a’
                    DC C’abcdefghi’
                    ORG TRANSLATE+C’j’
                    DC C’jklmnopqr’
                    ORG TRANSLATE+C’s’
                    DC C’stuvwxyz’
                    ORG TRANSLATE+C’A’
                    DC C’ABCDEFGHI’
                    ORG TRANSLATE+C’J’
                    DC C’JKLMNOPQR’
                    ORG TRANSLATE+C’S’
                    DC C’STUVWXYZ’
                    ORG ,

    결과적으로 TRANSLATE 테이블에는 아스키(ASCII) 기준 알파벳이 대문자와 소문자가 정렬되어 들어간다.

* END : <br>
  END 명령어를 사용하여 프로그램 어셈블리를 종료한다. 주소를 입력할 수도 있다. 프로그램이 로드된 후 제어가 전송될 수 있는 피연산자 필드이다. END 명령어는 항상 소스 프로그램의 마지막 명령문이어야 한다.

#### Program Section and Linking
* AMODE : <br>
  AMODE 명령어는 제어 섹션, ENTRY 기호 또는 EXTRN에서 사용할 주소 모드를 지정한다. 아래 AMODE 에 대한 포맷이다.

    ![amode_railload]({{site.baseurl}}/attach/amode_railload.png)

* RMODE : <br>
  RMODE 명령어는 프로그램의 제어 섹션을 로드할 때 주소 모드를 지정한다. 아래 RMODE 에 대한 포맷이다.

    ![rmode_railload]({{site.baseurl}}/attach/rmode_railload.png)

  - AMODE, RMODE 는 어셈블리의 아무 곳에서나 지정할 수 있다. 명명되지 않은 제어 섹션에서는 적용되지 않는다.

  - 어셈블리에는 여러 AMODE, RMODE 명령어가 있을 수 있다. 그러나 동일한 이름 필드에 대해서는 각각 하나의 AMODE 및 RMODE 명령어만 사용할 수 있다.

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
  CSECT 명령어는 실행 가능한 제어 섹션이거나 실행 가능한 제어 섹션의 연속을 나타낸다. 하나의 어셈블리 코드 내에서 CSECT는 여러 개 올 수 있고, 이는 프로그램 진입점(엔트리)이 여러 개 있다고 생각해도 좋다. 아래는 CSECT 에 대한 포맷이다.

  ![csect_railload]({{site.baseurl}}/attach/csect_railload.png)

  1. CSECT 명령어는 소스 모듈의 어느 곳에서나 사용할 수 있다. 첫 번째 실행 가능한 제어 섹션을 시작하는 데 사용되는 경우 위치 카운터에 영향을 미치므로 제어 섹션이 시작되도록 하는 명령이 선행되어서는 안된다.
  2. CSECT 의 name 필드는 제어 섹션을 식별한다. 소스 모듈 내의 여러 CSECT 명령어가 name 필드에 동일한 기호를 가지고 있는 경우 첫 번째 항목이 제어 섹션을 시작하고 나머지는 제어 섹션의 연속을 나타낸다. symbol로 표시되는 일반 심볼(Ordinary Symbol)은 제어 섹션의 첫 번째 바이트 주소를 나타내며 길이 속성 값이 1이다.
  3. 심볼이 지정되지 않았거나 시퀀스 심볼인 경우 CSECT 명령이 시작되거나 명명되지 않은 제어 섹션의 연속을 나타낸다. 첫 번째 제어 섹션이 START 명령에 의해 시작된 경우 섹션을 계속하는 CSECT 명령은 START 명령과 동일한 이름을 가져야 한다.
   
* DSECT : <br>
  더미 제어 섹션의 시작 또는 연속을 식별한다. 소스 모듈에서 하나 이상의 더미 섹션을 정의할 수 있다. 아래 DSECT 에 대한 포맷이다.

  ![dsect_railload]({{site.baseurl}}/attach/dsect_railload.png)

  1. symbol이 일반 심볼(Ordinary Symbol)을 나타내는 경우, 이 일반 심볼은 더미 섹션을 식별하기 위해 사용된다. 소스 모듈 내의 여러 DSECT 명령어의 이름 필드에 동일한 심볼이 있는 경우 첫 번째 항목은 더미 섹션을 시작하고 나머지는 더미 섹션의 연속을 나타낸다. symbol로 표시된 일반 심볼은 더미 섹션의 첫 번째 바이트 주소를 나타내며 길이 속성 값이 1이다. 
  2. symbol이 지정되지 않거나 이름이 시퀀스 symbol인 경우 DSECT 명령은 이름 없는 제어 섹션의 연속으로 취급된다.
  3. 더미 섹션에 대한 위치 카운터는 항상 초기값 0으로 설정된다. 그러나 중단된 더미 제어 섹션이 DSECT 명령을 사용하여 계속되면 해당 제어 섹션에서 마지막으로 지정된 로케이션 카운터가 계속된다.
  4. DSECT 명령어 뒤에 오는 소스 문은 해당 DSECT 명령어로 식별되는 더미 섹션에 속한다.
  5. 자세한 내용은 defining data 에서 다룬다.

#### Base Register
* DROP : <br>
  DROP 명령어는 USING 명령어의 베이스 레지스터 지정을 해제한다.
    - 다른 프로그래밍 목적으로 USING 명령어에 의해 이전에 할당된 베이스 레지스터를 해제한다.
    - 예를 들어 두 USING 범위가 겹치거나 일치하는 경우와 같이 특정 코딩 상황에서 어셈블러가 원하는 베이스 레지스터를 사용하기 다른 레지스터를 해제하는 경우가 있다.

* USING : <br>
  USING 명령어는 베이스 주소를 지정한다. 주소와 범위를 지정하고, 하나 이상의 베이스 레지스터를 할당한다. 베이스 주소를 로드하고 베이스 레지스터를 지정하면 제어 섹션 내 심볼에 대한 주소 접근이 가능해진다. 이를 일컬어 addressability를 생성(구성)한다고 말한다. 제어 섹션이 설정되지 않은 경우 USING은 명명되지 않은 제어 섹션으로 적용된다.

  USING 명령어를 올바르게 사용하려면 다음을 알아야 한다.
  * USING 명령어에 의해 제어 섹션의 어떤 위치 부터 addressability를 생성하는지
  * 소스 모듈에서 명령어 피연산자가 심볼일 경우 USING 에 의해 해당 심볼이 addressable 한지

#### Data Definition
* DC : <br>
  DC 명령어를 사용하여 프로그램 실행에 필요한 데이터 상수를 정의한다. DC 명령어는 어셈블러가 어셈블된 소스 모듈의 특정 위치에 지정하는 데이터 상수의 이진 표현을 생성하도록 한다. 이 작업은 어셈블리 타임에 이루어진다. 아래 DC 에 대한 포맷이다.

  ![dc_railload]({{site.baseurl}}/attach/dc_railload.png)

  DC는 단순히 프로그램 영역에 초기 데이터를 생성한다. 해당 값은 프로그램 실행 중에 수정될 수 있다. 그래서 런타임 중에 값을 보장할 수 없다. 값을 보장하는 상수처럼 작동하는 값을 선언하려면 리터럴을 사용해야 한다. 

  DC 명령어는 다음 유형의 상수를 생성할 수 있다.
  
  ![dc_type_table]({{site.baseurl}}/attach/dc_type_table.png)

* DS : <br>
  * 저장 공간 확보
  * 이 영역에 대한 레이블을 제공.
  * 레이블로 정의된 심볼을 참조하여 이러한 영역을 사용.
  * 제어 섹션이 이전에 설정되지 않은 경우 DS는 이름 없는 제어 섹션에 있는 것으로 적용.
  ![ds_railload]({{site.baseurl}}/attach/ds_railload.png)

  DS,DC의 Operand 는 6개의 하위 필드로 구성된 피연산자이다. 처음 5개의 하위 필드는 심볼의 속성을 설명한다. 여섯 번째 하위 필드는 암시적 길이를 결정하는 명목 값을 제공한다. 형태는 아래와 같다.
  ![ds_dc_operand_railload]({{site.baseurl}}/attach/ds_dc_operand_railload.png)

  DC 명령어와 달리 DS 명령어는 데이터를 어셈블(목적코드로 변환)하지 않는다. 따라서 DS 명령어 피연산자의 nominal value(위 그림에서 6 번째 서브 필드, 초기값을 의미)을 지정할 필요가 없다. DS 명령어는 작업 영역, 입력 및 출력 버퍼 등에 대한 저장소를 기호적으로 정의하는 가장 좋은 방법이다.
  nominal value 서브 필드는 DS 피연산자에서는 선택 사항이지만 DC 피연산자에서는 필수이다.

  길이값(Lengh modifier: L로 표기)이 명시적으로 기재되어 있는 경우 해당 값이 곧 데이터의 바이트 단위 사이즈가 되고, 명시적으로 기술되어 있지 않는 경우 각 데이터 타입의 기본 사이즈가 바이트 단위 사이즈가 된다. duplication factor 는 기본값이 1 이며 명시적으로 기술되어 있는 경우 뒤에 오는 유형의 반복여부를 결정한다. 배열이라 생각하면 편하다.

### Macro instructions
매크로 명령어는 매크로 정의라고 하는, 사전에 정의된 명령어 시퀀스를 처리하도록 어셈블러에 요청한다. 어셈블러는 매크로 명령어 처리에 의해 조건에 맞는 기계 및 어셈블러 명령어를 생성하여 어셈블리 소스의 일부분을 구성한다. 시스템에서는 입력/출력, 데이터 관리 및 기타 SVC(Supervisor call)에 대한 매크로 정의를 제공한다. 또한 고유한 매크로 정의를 작성하고 해당 매크로 명령어를 사용하여 이를 호출할 수 있다. 이를 유저 매크로라 한다. 필요할 때마다 이 모든 시퀀스를 코딩하는 대신, 시퀀스를 나타내는 매크로 명령을 만들어 이것이 ​​필요할 때마다 호출한다. 이는 코드 작성을 위한 비용을 줄일 수 있다.

매크로는 "MACRO" 구문으로 시작해야하며, "MEND" 구문으로 끝나야한다.
매크로 명령어는 아래와 같은 종류의 구문들로 구성된다.
* 모델 구문(Model statements)
* 프로세싱 구문(Processing statements)
* 주석 구문(Comment statements)
* 심볼릭 파라미터(Symbolic parameters)
* 시스템 변수 심볼(System variable symbols)

#### Model statements
모델 구문(Model statements)이란 Conditional assembly 를 통해 어셈블리 과정에서 어셈블러 언어가 생성되는 구문을 일컫는다. 모델 구문에서는 변수 심볼(Variable symbol)을 이용하여 다양한 형태의 구문을 생성할 수 있다. 모델 구문은 열 1 - 71에서 하나 이상의 공백으로 구분된 하나 이상의 필드로 구성된다. 필드는 Name, Operator, Operand 및 Remark 필드로 구성된다.

* 변수 심볼을 이용한 심볼 치환 : <br>
  모델 구문에서는 Remark 필드를 제외한 각 필드에 나타나는 변수 심볼을 미리 지정한 값으로 치환 할 수 있다. 변수 심볼은 주로 아래 세 가지 용도로 사용한다.

  * [Symbolic parameters(Positional parameter 또는 Keyword parameter)](#symbolic-parameters)
  * [System variable symbols](#system-variable-symbols)
  * SET symbols(전역 범위 또는 로컬 범위 SETA, SETB 또는 SETC 명령어를 위한 심볼)
    * *SET symbol에 대해서는 Language Reference의* ***Ch.9 How to write conditional assembly instructions, SET symbols*** *를 참조할 것.*

##### Rules for model statement fields
모델 구문에서 지정 할 수 있는 필드는 일반 어셈블러 언어 문에서 지정할 수 있는 필드(Name, Operation, Operand, Remarks 필드)와 동일하다. 유효한 어셈블러 명령어를 생성하려면 operation 필드가 있어야 하고, 대부분의 경우 operand 필드가 채워져 있어야 한다.

* Name field <br>
모델 구문의 name 필드에 허용되는 항목은 다음과 같다 : <br>
  * 공백 문자(Space)
  * 일반 심볼(Ordinary symbol)
  * 시퀀스 심볼(Sequence symbol)
  * 변수 심볼(Variable symbol)
  * 변수 심볼 또는 &SYSNDX와 같은 시스템 변수 심볼(System variable symbols) 및 함께 연결된 기타 문자열의 조합

* Operation field <br>
모델 구문의 Operation 필드에 허용되는 항목은 다음과 같다 : <br>
  * OPCODE를 나타내는 일반 심볼:
    * 모든 기계 명령어
    * 매크로 명령어
    * MNOTE 명령어
    * 변수 심볼
    * 다양한 문자열이 조합된 형태
    * ICTL 및 Conditional assembly 명령어를 제외한 모든 어셈블리 명령어
  * 모델 구문의 Operation 필드에는 아래 규칙이 적용 : <br>
    * MACRO 및 MEND 문은 모델 구문에서 허용되지 않는다. 매크로 정의를 구분하는 데만 사용된다.
    * 변수 심볼은 단독으로 사용하거나 연결된 문자열의 일부로 사용하여 다음에 대한 Operation 코드를 생성할 수 있다.
      * 모든 기계 명령
      * LCLx, GBLx, SETx, AIF 및 AGO와 같은 조건부 어셈블리 명령어
      * 다음 어셈블러 명령어: COPY, ICTL, ISEQ, MACRO, MEND, MEXIT 및 REPRO
    * 더블 바이트 데이터는 Operation 필드에서 유효하지 않는다.
  
* Operand field <br>
생성 전에 모델 구문의 Operand 필드에 허용되는 항목은 다음과 같다 : <br>
  * 공백 문자(Space)
  * 일반 심볼
  * 영숫자와 특수 문자를 결합한 문자열(변수 심볼 제외)
  * 변수 심볼
  * 서로 연결된 변수 심볼 및 기타 문자열의 조합
  * DBCS 어셈블러 옵션이 지정되면 ''' 묶인 문자열에 2바이트 데이터가 포함될 수 있다.

  허용 가능한 생성 결과는 공백 문자(유효한 경우)와 유효한 어셈블러, 기계 명령어 또는 매크로 명령어 Operand 필드를 나타내는 문자열이다.

  변수 심볼은 ICTL 또는 ISEQ 명령어의 Operand 필드에 사용하면 안 된다. 또한 매크로 정의 내부에 있는 COPY 명령어의 Operand 필드에는 변수 심볼을 사용해서는 안 된다.

#### Symbolic parameters
심볼릭 파라미터(Symbolic parameters)를 사용하면 호출하는 매크로 명령에서 매크로 정의의 본문으로 값을 전달할 수 있다. 매크로 프로토타입 문에서 이러한 파라미터를 선언한다. 매크로 정의의 본문에서 값을 대체하는 역할을 할 수 있으며 호출 매크로 명령에 의해 할당된 값으로 대체된다.
의미 있는 이름을 가진 심볼릭 파라미터를 사용하여 파라미터(또는 대체된 값)가 사용되는 목적을 나타낼 수 있다.
심볼릭 파라미터는 유효한 변수 심볼의 형태를 갖추어야 한다. 심볼릭 파라미터는 & 다음에 영문자와 0~61자의 영숫자로 구성된다.

유효한 심볼릭 파라미터는 다음과 같다 :

    &READER &LOOP2
    &A23456 &N
    &X4F2 &$4

다음은 잘못된 심볼릭 파라미터이다 :

    CARDAREA  -> 첫 번째 문자는 & 이여야 한다. 
    &256B -> & 뒤의 첫 번째 문자는 알파벳이어야 한다.
    &BCD%34 -> 초기 & 이외의 특수 문자를 포함하고 있다.
    &IN AREA -> 초기 & 이외의 특수 문자 [공백]을 포함하고 있다.

심볼릭 파라미터는 매크로 정의 내에서만 적용된다. 즉, 할당된 이름과 값은 선언된 매크로 정의에만 적용되는 로컬 스코프(Local scope) 성격을 가진다.
파라미터의 값은 해당 정의의 각 호출 동안 포함하는 매크로 정의를 처리하는 동안 일정하게 유지된다. 심볼릭 파라미터에는 아래 두가지 타입이 있다.

* Positional parameters <br>
매크로 정의를 호출할 때마다 파라미터 값을 변경하려면 매크로 정의에서 위치 파라미터(Positional parameter)를 사용하면 된다. 이는 키워드 파라미터보다 위치 파라미터에 대한 값을 제공하는 것이 더 쉽기 때문이다. 호출하는 매크로 명령어의 피연산자에서 해당 파라미터의 위치에 원하는 값만 쓰면 된다. 그러나 많은 파라미터가 필요한 경우 키워드 파라미터를 시용하는 것이 좋다. 키워드를 사용하면 값이 지정된 파라미터를 알려줌으로써 각 호출에서 지정해야 하는 개별 값을 쉽게 추적할 수 있다. 아래 관련 그림이다.

![positional_parameters]({{site.baseurl}}/attach/positional_parameters.png)

* Keyword parameters <br>
자주 변경되지 않는 값에 대해 매크로 정의에서 키워드 파라미터를 사용하거나 파라미터가 많은 경우 키워드 파라미터를 사용하는 것이 좋다. 피연산자에서 반복되는 키워드는 어떤 파라미터에 값이 주어지고 어떤 목적으로 파라미터가 사용되고 있는지 알기 쉽다. 키워드 파라미터에 할당할 표준 기본값을 지정하면 호출 매크로 명령어에서 해당 키워드 파라미터값을 생략할 수 있다. 매크로 명령어를 호출할 때 해당 키워드 피연산자를 임의의 순서로 지정할 수 있다. 아래 관련 그림이다.

![keyword_parameters]({{site.baseurl}}/attach/keyword_parameters.png)

#### Processing statements
이 섹션에서는 이러한 프로세싱 구문(Processing statements)에 대한 정보를 제공한다.

* Conditional assembly instructions <br>
조건부 어셈블리 명령어(Conditional assembly instructions)를 사용하면 생성할 구문의 내용과 그 순서를 조건부 어셈블리(Conditional assembly) 시간에 결정할 수 있다. 다음은 관련 명령어와 기능에 대한 간략한 설명이다. 

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
매크로 정의를 작성하는 데 도움이 될 수 있는 몇 가지 추가 명령어가 있다. 다음은 관련 명령어와 그 기능에 대한 간략한 설명이다 : <br>

| Inner Macro Instruction | Operation Done |
| ----------------------- | -------------- |
| AEJECT | 다음 statement 로 진행 |
| AINSERT | 입력 스트림에 statement  삽입 |
| AREAD | 변수 심볼에 임의의 문자열 할당 (SETC symbol) |
| ASPACE | 목록에 하나 이상의 빈 줄 삽입 | 
| COPY | 라이브러리으로 소스 복사 |
| MEXIT | 매크로 정의 종료 |

#### Comment statements
* Ordinary comment statements
일반 주석문을 사용하면 매크로 정의에서 생성된 출력에 대해 설명할 수 있다. 일반 주석문은 매크로 정의 및 공개 코드에서 사용할 수 있다. 일반 주석문은 시작 열의 * 와 그 뒤에 오는 문자열로 구성된다. 주석문은 어셈블러가 어셈블러 문을 생성하기 위해 다른 모델 구문을 사용하는 것처럼 어셈블러에서 어셈블러 언어 주석문을 생성하는 데 사용된다. 변수 심볼에 의한 문자열 치환 기능은 수행되지 않는다.

* Internal macro comment statements
매크로가 처리될 때 조건부 어셈블리 과정 중 수행되는 작업을 설명하기 위해 매크로 정의 본문에 내부 매크로 주석을 작성할 수도 있다. 내부 매크로 주석문은 시작 열의 마침표, *, 문자열로 구성된다. 내부 매크로 주석문에 지정된 변수 심볼는 값으로 치환되지 않는다. 내부 매크로 주석문은 매크로 정의의 아무 곳에나 나타날 수 있다.

#### System variable symbols
시스템 변수 심볼(System variable symbol)은 &SYS 문자로 시작하는 변수 기호의 특수 클래스이다. 각각의 값은 특정 규칙에 따라 어셈블러에 의해 설정된다. 로컬 범위 SET 기호 또는 전역 범위 SET 기호에서 선언하거나 매크로 프로토타입 문에서 심볼릭 파라미터로 사용할 수 없다. 이러한 기호를 모델 구문 및 조건부 어셈블리 명령어에서 변수 심볼과 같이 치환 point로 사용할 수 있다. 

모든 시스템 변수 심볼에는 다른 변수 심볼과 동일한 연결 및 치환 규칙이 적용된다. 
자세한 시스템 변수에 대해서는 [여기](https://www.ibm.com/docs/en/hla-and-tf/1.6?topic=definitions-system-variable-symbols)를 참조한다

## 용어집

  | 원문 | 번역 |
  | ---- | ---- |
  | Model statements | 모델 구문 |
  | Variable symbols | 변수 심볼 |
  | Ordinary symbols | 일반 심볼 |
  | Machine instructions | 기계 명령어 |
  | Instruction | 명령어 |
  | Assembler instructions | 어셈블러 명령어 |
  | Macro instructions | 매크로 명령어 |
  | Object code | 목적코드 |
  | Register | 레지스터 |
  | Base register | 베이스 레지스터 |
  | Base address | 베이스 주소 |
  | Displacement | 변위값 |
  | Index register | 인덱스 레지스터 |
  | Operand | operand 혹은 피연산자 |
  | Location counter | 로케이션 카운터 |
  | System variable symbol | 시스템 변수 심볼 |
  | Conditional assembly instruction | 조건부 어셈블리 명령어 |
  | Substitution | 치환 |
  | Symbolic parameter | 심볼릭 파라미터 |
  | Keyword parameter | 키워드 파라미터 |
  | Parameter | 파라미터 |