---
title: Assembler 데이터 선언
category: ASM Programming Introduction
order: 4
---

## 목차

- [목차](#목차)
- [Assembler Defining Data](#assembler-defining-data)
  - [Define Storage (DS) Instructions](#define-storage-ds-instructions)
  - [Define Constant (DC) Instruction](#define-constant-dc-instruction)
  - [Length Modifier](#length-modifier)
  - [Length Attribute (L')](#length-attribute-l)
  - [Duplication Factor](#duplication-factor)
  - [Dummy Sections](#dummy-sections)
  
----
## Assembler Defining Data
### Define Storage (DS) Instructions
The Define Storage (DS) 어셈블러 명령은 프로그램 내의 저장 영역을 정의하는 데 사용된다. 정의 중인 스토리지에 이름을 지정하려면 DS 명령어의 Name 필드에 기호를 지정한다. 스토리지를 정의하는 작업은 DS 이다. DS 문의 피연산자 필드는 다음과 같은 최대 4개의 구성요소로 구성된다.

* duplication factor
* type specifier
* length modifier
* Length Attribute
* Nominal value

각 타입 지정자는 아래와 같은 각 키워드로 존재하며 다른 세 구성 요소는 선택 사항이다. 명시적으로 코딩되지 않은 경우 duplication factor 및 length modifier는 기본값을 갖는다. nominal value 필드는 사용되지 않으면 상수가 아닌 storage를 정의를 의미한다. 아래 관련 그림이다.

![ds_components]({{site.baseurl}}/attach/ds_components.png)

타입 지정자는 필드에 포함되어야 하는 데이터 타입을 나타낸다. 각 데이터 타입은 단일 문자 식별자로 지정되며 기본 길이와 alignment가 있다. 가장 일반적인 필드 타입은 아래와 같다.

![data_type]({{site.baseurl}}/attach/data_type.png)

예를들어 FWD1이라는 fullword 를 정의하려면 FWD1 DS F 와 같이 정의한다.

### Define Constant (DC) Instruction
The Define Constant (DC) 명령은 상수를 정의하는 데 사용된다. 사실 상수라는 용어는 DC 어셈블러 명령어에 의해 설정된 저장소가 프로그램 실행 내내 일정하게 유지된다는 보장이 없기 때문에 다소 오해의 소지가 있다. 실제로 DC가 하는 일은 스토리지를 정의하고 DC 명령어에서 지정한 값으로 초기화하는 것이다.

DC의 피연산자 필드는 DS 명령어와 동일한 4가지 구성 요소를 포함하지만 nominal value 필드는 필수이다. nominal value 값은 필드가 초기화되는 값을 지정한다.

### Length Modifier
Length Modifier 는 다음과 같이 구분할 수 있다.
* Byte-length modifiers : <br>
modifiers의 값은 상수에 할당된 스토리지의 바이트 수를 결정한다. Ln으로 작성되며, 여기서 n은 10진수 자체 정의 용어 또는 괄호로 묶인 절대 표현식이다. 양의 값을 가져야 한다. Length Modifier가 지정된 경우:
  - 해당 값은 상수에 할당된 스토리지의 바이트 수를 결정한다. 따라서 상수의 nominal value을 할당된 공간에 맞게 채워야 하는지 또는 잘려야 하는지를 결정한다.
  - 상수 type에 따라 boundary alignment 를 맞추지 않는다.
  - 해당 값은 정의된 다양한 type의 상수에 대해 허용되는 최대 길이를 초과하지 않아야 한다.
  - Length Modifier는 bit-length modifiers를 제외하고 C type 상수에서 2바이트 데이터(DBCS)를 자르지 않아야 한다.
  - Length Modifier는 G type 또는 CU type 상수에서 2의 배수여야 한다.

* Bit-length modifiers : <br>
length modifiers는 상수가 조합될 비트 수를 나타내기 위해 지정할 수 있다. Bit-length modifiers는 L.n으로 작성됩니다. 여기서 n은 10진수 자체 정의 용어이거나 괄호로 묶인 절대 표현식이다 양의 값을 가져야 한다. 이러한 modifiers는 "Byte-length" 수정자와 구별하기 위해 "Bit-length" modifiers라고도 한다. Byte-length와 Bit-length modifiers를 결합할 수 없다. 예를 들어, 12비트 필드는 L1.4가 아니라 L.12로 작성되어야 한다. n의 값은 1 - 정의되는 상수 type에서 허용되는 최대 바이트 수를 구성하는 데 필요한 비트 수(8의 배수)이다. Bit-length modifiers는 CU, G, S, V, R, J 및 Q 타입 상수와 함께 사용할 수 없으며 다음과 같은 경우 A 타입 또는 Y 타입 상수와 함께 사용할 수 없다. DC 명령어에 하나의 피연산자와 하나의 nominal value만 지정된 경우 다음 규칙이 적용된다 : <br>

  - 할당된 첫 번째 필드는 바이트 경계에서 시작하지만 다음 필드는 사용 가능한 다음 비트에서 시작한다. 예를 들어, BL1 DC FL.12'-1,1000'은 X'FFF3E8'을 의미한다.
  - 모든 상수가 해당 필드에 조합된 후 마지막 바이트를 구성하기 위해 남아 있는 비트는 0으로 채워진다. 예를 들어, BL2 DC FL.12'-1,1000,-2'는 X'FFF3E8FFE0'을 의미한다 duplication factor가 지정되면 상수가 차지하는 모든 필드의 끝에서 0으로 채우워진다. 예를 들어, BL3 DC 3FL.12'-2'는 X'FFFFFEFFE0'을 의미한다.
  - DC 명령어를 명명하는 symbol의 길이 속성 값은 지정된 비트 길이를 나타내기 위해 필요한 정수 바이트 수와 같다. 예를 들어, 앞의 예에서 기호 BL1, BL2 및 BL3은 각각 길이 속성 2를 가진다.

### Length Attribute (L')
Length Attribute에는 속성 참조에 지정된 기호로 명명된 데이터가 차지하는 바이트를 가지고 있다. 다음은 이 구분을 명확히 하는 예이다 : <br>

    &B SETC ’B’
    AB DC C’A&B’          Valid in ordinary assembly
    LAB DC AL1(L’A&B)     Valid in ordinary assembly
    &N SETA L’A&B         Invalid in conditional assembly
    &T1 SETB (L’A&B EQ 2) Invalid in conditional assembly
    &T2 SETB (2 EQ L’A&B) Invalid in conditional assembly

어셈블러가 SETB 식을 분석하는 동안 오류가 감지되기 때문에 두 SETB 문은 서로 다른 진단 메시지를 받습니다. conditional assembly 문에서  length attribute  참조의 피연산자는 문자 식이 아니라 일반 또는 가변 기호여야 합니다. length attribute 은 conditional assembly 명령 외부에서도 지정할 수 있다. 그리고, length attribute 값은 conditional assembly 처리 시에 이용이 불가능하나, assembly time에 값으로서 사용된다.

아래 예에서 첫번째 명령문은 assembler instruction 에서 length attribute 쓰임 예이고, 여덟번쨰 명령문은 conditional assembly instruction 에서 length attribute 쓰임의 예이다.

![length_attribute]({{site.baseurl}}/attach/length_attribute.png)

두번쨰 명령문에서 첫번쨰 명령문의 CSYM 정의가 완전하지 않기 때문에 CSYM의 길이가 설정되지 않는다. length attribute에 대한 참조는 길이가 1이고 오류 메시지 ASMA042E가 된다.
그러나 다섯번쨰 명령문은 타입 속성이 할당되었음을 나타내고 일곱번째 명령문은 정의된 속성이 할당되었음을 나타낸다. 이에 비해 기호 CSYM2의 length attribute은 여덟번쨰 명령문의 conditional assembly 명령을 사용하여 간접적으로 검색되었기 때문에 즉시 사용할 수 있다.

### Duplication Factor
Duplication Factor를 생략할 수 있다. 지정된 경우 상수에 지정된 nominal values 또는 여러 nominal values이 계수로 표시된 횟수만큼 생성된다. nominal value 또는 값을 상수로 조합한 후 적용된다.리터럴에는 적용되지 않습니다. Duplication Factor는 부호 없는 10진수 자체 정의 용어 또는 괄호로 묶인 절대 표현식으로 지정할 수 있다. 계수는 양수 값을 가지거나 0과 같아야 한다.

1. 리터럴을 제외하고 복제 계수 0이 허용되며 결과는 다음과 같다.
   * Length attribute가 없는 경우 지정된 상수 타입에 따라 강제로 alignment 된다.
   * 상수를 명명하는 기호의 length attribute은 내재적으로 또는 명시적으로 지정된 길이에 따라 설정된다. 
   * Duplication Factor가 0이면 nominal value을 생략할 수 있다. Duplication Factor가 리터럴에 대해 0이면 어셈블러는 ASMA067S Illegal duplication factor 메시지를 발행한다.

2. nominal value에 location counter 참조가 포함된 주소 상수에 대해 중복이 지정되면 location counter 참조 값은 상수의 길이만큼 증가한다. 각 복제가 완료되기 전에 duplication factor가 0이면 location counter 참조 값은 0이 아닌 duplication factor에 대해 생성된 각 상수의 길이만큼 증가하지 않는다. 따라서 다음 두 명령문에서 첫 번째 명령문은 "Data item too large"에 대한 ASMA072E 오류 메시지를 생성하지만 두 번째 명령문은 생성하지 않는다.
   
        A DC 0Y(0,32768-(*-A))
        B DC Y(0,32768-(*-B))

    그러나 location counter 참조를 포함하는 주소 타입 리터럴 상수에 대해 복제가 지정되면 각 복제가 완료되기 전에 location counter 참조 값이 리터럴 길이만큼 증가하지 않는다. location counter 참조 값은 literal pool 에 있는 리터럴의 첫 번째 바이트 위치이며 각 중복에 대해 동일하다. location counter 값은 A-type 경우 리터럴이 나타나는 명령어의 값이지만 S-type 경우 리터럴이 나타나는 위치이다.

    A, B, F, H, P, X, Y 또는 Z 유형의 비트 길이 상수가 복제 계수와 함께 지정된 경우:
    
        각 nominal value은 지정된 필드에서 오른쪽으로 정렬된다.
        각 nominal value은 type.에 따라 왼쪽에 0 또는 부호 비트가 채워진다.

    각 상수가 생성된 후 채워지지 않은 비트가 남아 있으면 마지막 바이트의 나머지 비트는 0비트로 채워진다.

3. 비트 길이 상수가 duplication factor와 함께 지정되면 각 nominal value은 지정된 필드에서 오른쪽으로 정렬되고 왼쪽은 타입에 따라 0 또는 부호 비트로 채워진다. 각 상수가 생성된 후 채워지지 않은 비트가 남아 있으면 마지막 바이트의 나머지 비트는 0비트로 채워진다. 따라서 상수 내의 패딩은 상수 그룹 뒤의 패딩과 다르다.

4. Duplication factor의 최대값은 2^24-1 또는 OBJ 개체 파일의 경우 X'FFFFFF', GOFF 개체 파일의 경우 2^32-1 또는 X'7FFFFFF'이다. duplication factor의 최대값을 초과하면 어셈블러에서 메시지를 발행한다. ASMA067S 불법 복제 요소 및 ASMA068S 길이 오류가 발생할 수 있다.

* Zero duplication factor 를 사용하는 일반적인 이유: <br>
  한 가지 이유는 정렬을 강제하는 것이다. double word 경계에 정렬된 100바이트 문자 필드가 정의된다고 가정할때, 이를 수행하는 가장 쉬운 방법은 문자 필드 바로 다음에 오는 double word(double word 정렬 강제)를 정의하는 것이다. double word는 강제 정렬 이외의 다른 용도로 실제로 사용되지 않기 때문에 duplication factor 0으로 정의된다. 사용되지 않기 때문에 이름을 지정할 필요도 없다. 유일한 목적은 어셈블러가 double word 경계로 이동하기에 충분한 바이트를 건너뛰게 하는 것이다. 아래 관련 그림이다.

  ![force_alignment]({{site.baseurl}}/attach/force_alignment.png)

  두번째 이유로는 동일한 영역을 둘 이상의 다른 방식으로 정의될 때 zero duplication factor가 자주 사용된다. 저장 영역이 디스크에서 프로그램에서 읽을 레코드 버퍼가 있다고 가정할때, 레코드의 길이는 120바이트이지만 이 레코드에 길이가 각각 40바이트인 3개의 필드가 있다고 가정한다. 레코드를 읽을 때 이를 단일 120바이트 필드로 읽은 후 해당 필드를 별도로 처리하기를 원하는 경우 첫 번째 정의에서 zero duplication factor를 사용하여 수행할 수 있다. 아래 관련 그림이다.

  ![zero_duplication_factor]({{site.baseurl}}/attach/zero_duplication_factor.png)
  
  한가지 추가적인 예로 아래 그림처럼 RECORD는 0 duplication factor로 정의되며 할당된 실제 공간은 0 * 120 = 0 이다. RECORD의 length attribute은 120이다. FLD1은 RECORD와 같은 위치를 갖지만 length attribute은 40이다. 어셈블러가 할당한 공간은 1 * 40 = 40이다. FLD2의 주소는 FLD1의 주소보다 40 더 높다. FLD2도 FLD3과 마찬가지로 length attribute이 40이다. 필드는 더 세분화할 수 있다. FLD1은 4개의 작은 필드로 구성될 수 있다. 아래 관련 그림이다.

  ![zero_duplication_factor2]({{site.baseurl}}/attach/zero_duplication_factor2.png)

### Dummy Sections
DSECT 명령은  Dummy Control Sections의 시작 또는 연속을 식별한다. 소스 모듈에서 하나 이상의 Dummy Sections을 정의할 수 있다.

기호가 일반 기호를 나타내는 경우 일반 기호는 Dummy Section을 식별한다. 소스 모듈 내의 여러 DSECT 명령어의 Name field에 동일한 기호가 있는 경우 첫 번째 항목은 Dummy Section을 시작하고 나머지는 더미 섹션의 연속을 나타낸다. symbol로 표시되는 일반 심볼은 Dummy Section의 첫 번째 바이트 주소를 나타내며 length attribute 값이 1이다.

기호가 지정되지 않았거나 이름이 시퀀스 기호인 경우 DSECT 명령은 명명되지 않은 control section의 연속을 시작하거나 나타낸다. Dummy Section에 대한 위치 카운터는 항상 초기 값 0으로 설정된다. 그러나 중단된 Dummy Control Section이 DSECT 명령을 사용하여 계속되면 해당 control section에서 마지막으로 지정된 location counter가 계속된다. DSECT 명령어 뒤에 오는 소스 문은 해당 DSECT 명령어로 식별되는 Dummy Section에 속한다.

한 예로 두 개의 독립 어셈블리(Assembly-1 및 Assembly-2)가 로드되어 단일 전체 프로그램으로 실행된다고 가정헸을떄, Assembly-1은 다음과 같은 루틴이다.
1. 저장 영역에 레코드를 놓는다.
2. 일반 레지스터 3에 저장 영역의 주소를 둔다.
3. Assembly-2로 분기하여 레코드 처리한다.

Assembly-1의 저장 영역은 IAREA라는 Dummy Control Section(DSECT)에 의해 Assembly-2에서 식별된다. 작업하려는 저장 영역 부분의 이름은 INCODE, OUTPUTA 및 OUTPUTB 이다. USING IAREA,3 명령문은 일반 레지스터 3을 IAREA DSECT에 대한 기본 레지스터로 지정한다. 일반 레지스터 3은 저장 영역의 주소를 포함한다. DSECT의 기호는 DSECT의 시작 부분을 기준으로 정의된다. 이것은 그들이 나타내는 주소 값이 프로그램 실행 시 일반 레지스터 3이 주소를 지정하는 저장 영역의 실제 저장 위치임을 의미한다. 아래 예이다.

![example_dummy_section]({{site.baseurl}}/attach/example_dummy_section.png)

때때로 처리된 매개변수는 단순한 변수가 아닌 복잡한 데이터 구조일수가 있다. 이름, 두 줄의 주소, 시, 도, 우편번호로 구성된 고객 레코드의 경우를 매개변수로 할때, 여기의 예는 그러한 레코드 중 하나를 보여준다. 이 레코드가 정의된 모듈에서 전체 레코드 또는 해당 필드 중 하나를 참조할 수 있다. 전체 레코드를 duplication factor가 0으로 정의하는 기술을 사용했기 때문이다. 아래 예이다.

![example_dummy_section2]({{site.baseurl}}/attach/example_dummy_section2.png)

이 고객 레코드가 다른 모듈로 전달되는 경우를 고려하면 여기서 발생하는 문제는 호출된 모듈에서 데이터에 액세스할 필요가 있지만 여기에 정의되어 있지 않다는 것이다. 이 경우 실제로 스토리지를 설정하지 않고 데이터 구조를 지정해야 한다. 스토리지가 이미 다른 모듈에 존재하기 때문이다. 이는 Dummy Section(DSECT)으로 해결된다. 아래 예이다.

![example_dummy_section3]({{site.baseurl}}/attach/example_dummy_section3.png)

DSECT를 사용하면 스토리지를 할당하지 않고 이름과 관계를 정의하는 구조인 스토리지 템플릿을 정의할 수 있다. 고객 레코드의 필드를 나타내는 DSECT는 예제와 같이 별도로 조립된 호출된 모듈에서 정의할 수 있다. 아래 예이다.

![example_dummy_section4]({{site.baseurl}}/attach/example_dummy_section4.png)

* How to use DSECT

한가지 예를 들어 CREC 또는 해당 필드를 참조하기 전에 주소를 지정할 수 있어야 한다. 이것은 USING 명령어로 수행된다.

CREC의 실제 저장소 주소가 매개변수 목록에 전달되면 다음 코드를 사용하여 주소를 지정할 수 있다. 아래 예이다.

      L R3,0(,R1) R3             points to CREC
      USING CREC, R3             R3 makes the CREC addressable

![example_dummy_section5]({{site.baseurl}}/attach/example_dummy_section5.png)

이때 CREC의 필드를 이름으로 참조할 수 있다. 어셈블러는 CCITY에 대한 참조를 70(15,R3)으로 변환한다. 이는 CCITY가 CREC를 넘어 70바이트이고 어셈블러에서 R3에 CREC의 기본 주소가 포함되어 있기 때문이다. 동일한 기계어 코드는 DSECT 없이 생성될 수 있다. DSECT를 사용하면 기본 변위 주소가 아닌 기호를 사용하기 때문에 코드가 더 명확해진다. DSECT의 사용은 다른 모듈에서 정의되고 매개변수로 전달되는 구조화된 데이터를 처리하는 동안 항상 고려되어야 한다. 아래 예이다.

![example_dummy_section6]({{site.baseurl}}/attach/example_dummy_section6.png)