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
The Define Storage (DS) 어셈블러 명령은 프로그램 내의 저장 영역을 정의하는 데 사용된다. 정의 중인 스토리지에 이름을 지정하려면 DS 명령어의 Name 필드에 기호를 지정한다. DS 문의 피연산자 필드는 다음과 같은 최대 4개의 구성요소로 구성된다.

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
* Byte-length modifiers : <br>
modifiers의 값은 상수에 할당된 스토리지의 바이트 수를 결정한다. Ln으로 작성되며, 여기서 n은 10진수 자체 정의 용어 또는 괄호로 묶인 절대 표현식이다. 양의 값을 가져야 한다. Length Modifier가 지정된 경우:
  - 해당 값은 상수에 할당된 스토리지의 바이트 수를 결정한다. 따라서 상수의 nominal value을 할당된 공간에 맞게 채워야 하는지 또는 잘려야 하는지를 결정한다.
  - 상수 type에 따라 boundary alignment 를 맞추지 않는다.
  - 해당 값은 정의된 다양한 type의 상수에 대해 허용되는 최대 길이를 초과하지 않아야 한다.
  - Length Modifier는 bit-length modifiers를 제외하고 C type 상수에서 2바이트 데이터(DBCS)를 자르지 않아야 한다.
  - Length Modifier는 G type 또는 CU type 상수에서 2의 배수여야 한다.

* Bit-length modifiers : <br>
  - 해당 특성은 스펙이 존재하나 별도로 기술하지 않도록 한다.

### Length Attribute (L')
Length Attribute에는 속성 참조에 지정된 기호로 명명된 데이터가 차지하는 바이트를 가지고 있다. 아래는 Length attribute를 사용하는 예제 코드이다. <br>

  TEST    DC    C'LENGTH TEST'
  STRLENG DC    F'L'TEST'

위 예제를 Conditional Assembly 한 결과는, 심볼 TEST가 가리키는 데이터의 길이가 11바이트이므로, 아래와 같다.

  TEST    DC    C'LENGTH TEST'
  STRLENG DC    F'11'

### Duplication Factor
복제 계수(Duplication Factor)는 생략할 수 있다. 지정된 경우 상수에 지정된 하나의 명시값(nominal value) 또는 여러 명시값들이 계수로 표시된 횟수만큼 생성된다. 리터럴에는 적용되지 않는다. 복제 계수는 부호 없는 10진수 자체  또는 괄호로 묶인 절대 표현식으로 지정할 수 있다. 계수는 양수 값을 가지거나 0과 같아야 한다.

1. 리터럴을 제외하고 복제 계수 0이 허용되며 결과는 다음과 같다.
   * 길이 특성(Length modifier)이 없는 경우 지정된 상수 타입에 따라 강제로 정렬(alignment)된다.
   * 상수를 명명하는 심볼의 길이 특성은 내재적으로 또는 명시적으로 지정된 길이에 따라 설정된다. 
   * 복제 계수가 0이면 초기값을 생략할 수 있다. 복제 계수가 리터럴에 대해 0이면 어셈블러는 ASMA067S Illegal duplication factor 메시지를 발행한다.

2. 명시값에 로케이션 카운터 참조가 포함된 주소 상수에 대해 복제 계수가 지정되면 **로케이션 카운터 참조 값은 상수의 길이만큼 증가**한다. 각 복제가 완료되기 전에 복제 계수가 0이면 로케이션 카운터 참조 값은 0이 아닌 복제 계수에 대해 생성된 각 상수의 길이만큼 증가하지 않는다. 따라서 다음 두 명령문에서 첫 번째 명령문은 "Data item too large"에 대한 ASMA072E 오류 메시지를 생성하지만 두 번째 명령문은 생성하지 않는다.
   
        A DC 0Y(0,32768-(*-A))
        B DC Y(0,32768-(*-B))

    그러나 로케이션 카운터 참조를 포함하는 주소 타입 리터럴 상수에 대해 복제가 지정되면 각 복제가 완료되기 전에 로케이션 카운터 참조 값이 리터럴 길이만큼 증가하지 않는다. 로케이션 카운터 참조 값은 리터럴풀 에 있는 리터럴의 첫 번째 바이트 위치이며 각 중복에 대해 동일하다. 로케이션 카운터 값은 A-type 경우 리터럴이 나타나는 명령어의 값이지만 S-type 경우 리터럴이 나타나는 위치이다.

    A, B, F, H, P, X, Y 또는 Z 유형의 비트 길이 상수가 복제 계수와 함께 지정된 경우:
    
        각 명시값은 지정된 필드에서 오른쪽으로 정렬된다.
        각 명시값은 type.에 따라 왼쪽에 0 또는 부호 비트가 채워진다.

    각 상수가 생성된 후 채워지지 않은 비트가 남아 있으면 마지막 바이트의 나머지 비트는 0비트로 채워진다.

* Zero duplication factor 를 사용하는 일반적인 이유: <br>
  한 가지 이유는 정렬을 강제하는 것이다. double word 경계에 정렬된 100바이트 문자 필드가 정의된다고 가정할때, 이를 수행하는 가장 쉬운 방법은 문자 필드 바로 다음에 오는 double word(double word 정렬 강제)를 정의하는 것이다. double word는 강제 정렬 이외의 다른 용도로 실제로 사용되지 않기 때문에 duplication factor 0으로 정의된다. 사용되지 않기 때문에 이름을 지정할 필요도 없다. 유일한 목적은 어셈블러가 double word 경계로 이동하기에 충분한 바이트를 건너뛰게 하는 것이다. 아래 관련 그림이다.

  ![force_alignment]({{site.baseurl}}/attach/force_alignment.png)

  두 번째 이유로는 동일한 영역을 둘 이상의 다른 방식으로 정의될 때 zero duplication factor가 자주 사용된다. 저장 영역이 디스크에서 프로그램에서 읽을 레코드 버퍼가 있다고 가정할때, 레코드의 길이는 120바이트이지만 이 레코드에 길이가 각각 40바이트인 3개의 필드가 있다고 가정한다. 레코드를 읽을 때 이를 단일 120바이트 필드로 읽은 후 해당 필드를 별도로 처리하기를 원하는 경우 첫 번째 정의에서 zero duplication factor를 사용하여 수행할 수 있다. 아래 관련 그림이다.

  ![zero_duplication_factor]({{site.baseurl}}/attach/zero_duplication_factor.png)
  
  한 가지 추가적인 예로 아래 그림처럼 RECORD는 zero duplication factor로 정의되며 할당된 실제 공간은 0 * 120 = 0 이다. RECORD의 length attribute은 120이다. FLD1은 RECORD와 같은 위치를 갖지만 length attribute은 40이다. 어셈블러가 할당한 공간은 1 * 40 = 40이다. FLD2의 주소는 FLD1의 주소보다 40 더 높다. FLD2도 FLD3과 마찬가지로 length attribute이 40이다. 필드는 더 세분화할 수 있다. FLD1은 4개의 작은 필드로 구성될 수 있다. 아래 관련 그림이다.

  ![zero_duplication_factor2]({{site.baseurl}}/attach/zero_duplication_factor2.png)

### Dummy Sections
DSECT 명령은 더미 제어 섹션(Dummy control sections)의 시작 또는 연속을 식별한다. 소스 모듈에서 하나 이상의 더미 섹션(Dummy sections)을 정의할 수 있다.

심볼이 일반 심볼을 나타내는 경우 일반 심볼은 더미 섹션을 식별한다. 소스 모듈 내의 여러 DSECT 명령어의 Name field에 동일한 심볼이 있는 경우 첫 번째 항목은 더미 섹션을 시작하고 나머지는 더미 섹션의 연속을 나타낸다. symbol로 표시되는 일반 심볼은 더미 섹션의 첫 번째 바이트 주소를 나타내며 length attribute 값이 1이다.

심볼이 지정되지 않았거나 이름이 시퀀스 기호인 경우 DSECT 명령은 명명되지 않은 제어 섹션의 연속을 시작하거나 나타낸다. 더미 섹션에 대한 로케이션 카운터는 항상 초기 값 0으로 설정된다. 그러나 중단된 더미 제어 섹션이 DSECT 명령을 사용하여 계속되면 해당 제어 섹션에서 마지막으로 지정된 로케이션 카운터가 계속된다. DSECT 명령어 뒤에 오는 소스 문은 해당 DSECT 명령어로 식별되는 더미 섹션에 속한다.

한 예로 두 개의 독립 어셈블리(Assembly-1 및 Assembly-2)가 로드되어 단일 전체 프로그램으로 실행된다고 가정헸을떄, Assembly-1은 다음과 같은 루틴이다.
1. 저장 영역에 레코드를 놓는다.
2. 범용 레지스터 3에 저장 영역의 주소를 둔다.
3. Assembly-2로 분기하여 레코드 처리한다.

Assembly-1의 저장 영역은 IAREA라는 더미 제어 섹션(DSECT)에 의해 Assembly-2에서 식별된다. 작업하려는 저장 영역 부분의 이름은 INCODE, OUTPUTA 및 OUTPUTB 이다. USING IAREA,3 명령문은 범용 레지스터 3을 IAREA DSECT에 대한 베이스 레지스터로 지정한다. 범용 레지스터 3은 저장 영역의 주소를 포함한다. DSECT의 기호는 DSECT의 시작 부분을 기준으로 정의된다. 이것은 그들이 나타내는 주소 값이 프로그램 실행 시 범용 레지스터 3이 주소를 지정하는 저장 영역의 실제 저장 위치임을 의미한다. 아래 예이다.

![example_dummy_section]({{site.baseurl}}/attach/example_dummy_section.png)

때때로 처리된 파라미터는 단순한 변수가 아닌 복잡한 데이터 구조일수가 있다. 이름, 두 줄의 주소, 시, 도, 우편번호로 구성된 고객 레코드의 경우를 파라미터로 할때, 여기의 예는 그러한 레코드 중 하나를 보여준다. 이 레코드가 정의된 모듈에서 전체 레코드 또는 해당 필드 중 하나를 참조할 수 있다. 전체 레코드를 duplication factor가 0으로 정의하는 기술을 사용했기 때문이다. 아래 예이다.

![example_dummy_section2]({{site.baseurl}}/attach/example_dummy_section2.png)

이 고객 레코드가 다른 모듈로 전달되는 경우를 고려하면 여기서 발생하는 문제는 호출된 모듈에서 데이터에 액세스할 필요가 있지만 여기에 정의되어 있지 않다는 것이다. 이 경우 실제로 스토리지를 설정하지 않고 데이터 구조를 지정해야 한다. 스토리지가 이미 다른 모듈에 존재하기 때문이다. 이는 더미 섹션(DSECT)으로 해결된다. 아래 예이다.

![example_dummy_section3]({{site.baseurl}}/attach/example_dummy_section3.png)

DSECT를 사용하면 스토리지를 할당하지 않고 이름과 관계를 정의하는 구조인 스토리지 템플릿을 정의할 수 있다. 고객 레코드의 필드를 나타내는 DSECT는 예제와 같이 별도로 조립된 호출된 모듈에서 정의할 수 있다. 아래 예이다.

![example_dummy_section4]({{site.baseurl}}/attach/example_dummy_section4.png)

* How to use DSECT

한가지 예를 들어 CREC 또는 해당 필드를 참조하기 전에 주소를 지정할 수 있어야 한다. 이것은 USING 명령어로 수행된다.

CREC의 실제 저장소 주소가 파라미터 목록에 전달되면 다음 코드를 사용하여 주소를 지정할 수 있다. 아래 예이다.

      L R3,0(,R1) R3             points to CREC
      USING CREC, R3             R3 makes the CREC addressable

![example_dummy_section5]({{site.baseurl}}/attach/example_dummy_section5.png)

이때 CREC의 필드를 이름으로 참조할 수 있다. 어셈블러는 CCITY에 대한 참조를 70(15,R3)으로 변환한다. 이는 CCITY가 CREC를 넘어 70바이트이고 어셈블러에서 R3에 CREC의 기본 주소가 포함되어 있기 때문이다. 동일한 기계어 코드는 DSECT 없이 생성될 수 있다. DSECT를 사용하면 기본 변위 주소가 아닌 기호를 사용하기 때문에 코드가 더 명확해진다. DSECT의 사용은 다른 모듈에서 정의되고 파라미터로 전달되는 구조화된 데이터를 처리하는 동안 항상 고려되어야 한다. 아래 예이다.

![example_dummy_section6]({{site.baseurl}}/attach/example_dummy_section6.png)