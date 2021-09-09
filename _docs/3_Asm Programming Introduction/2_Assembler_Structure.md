---
title: Assembler 구조
category: ASM Programming Introduction
order: 2
---

## 목차

- [목차](#목차)
- [Assembler Structure](#assembler-structure)
  - [Register](#register)
    - [Gerenal Purpose Register](#gerenal-purpose-register)
  - [SaveArea](#savearea)
  - [Parameter List](#parameter-list)

----

## Assembler Structure
### Register

Register는 일반적으로 CPU가 가장 빠르게 접근할 수 있는 저장 공간 중 하나이다. 
Register에는 여러 종류가 있는데, 그 중 OFASM에서는 아래의 Register를 지원한다.

* General Purpose Register(GPR)
* Floating Point Register

#### General Purpose Register

General Purpose Register(GPR)는 프로그래머가 Assembly 프로그램에서 사용할 수 있도록 제공되는 4바이트 길이 Register 들을 말한다. 프로그래머는 일반적으로 데이터의 저장, 주소 지정, 제어권 전달 등의 기능을 위해 GPR을 사용한다.

아래는 레지스터 종류에 대한 그림이다. 주황색은 특수 목적 레지스터를 의미하며 파란색은 일반 목적 레지스트를 의미한다.

![레지스터]({{site.baseurl}}/attach/register.png)

64비트와 32비트의 모든 컴퓨터에서 지원하기 위해 각 Register들은 총 길이 8바이트로 지정된다. 가장 왼쪽 4바이트는 “0x00(null)”로 채워진다.

아래는 5개 레지스터는 모듈간 제어 및 데이터를 전달하고 반환 할때 사용된다.

1. GPR 0 : Register 0는 0 값을 의미하여 4096바이트 스토리지의 처음을 참조 할 수 있다. 아래 예제에서 R0의 값을 변경한 후에 R9에 어떤 값이 들어가는지를 확인해보면 명확해진다. 4번째 줄의 첫 LA문을 통해 R0에 들어간 값은 변경되지만, 다음 LA 시 R0 는 여전히 0으로 간주됨을 확인할 수 있다.

        MAINPROC EQU   *
                USING PARALIST,R7
                L     R7,0(R1)             R7 = R1의 첫 번째 인자의 주소가 저장
                LA    R0,2(R0,R7)          R0 = R7의 주소 + 0(Index) + 2(Displacement) = R7 + 2
                LA    R9,2(R0,R7)          R9 = R7의 주소 + 0(Index) + 2(Displacement) = R7 + R0(?) + 2

 1. GPR 1 : Register 1은 파라미터 리스트의 주소를 가지고 있다. 자세한 내용은 Parameter Lists 에서 확인한다.

 2. GPR 13 : SaveArea 주소를 유지하는 데 사용된다. 레지스터를 저장하는 데 사용되는 영역을 SaveArea 라고 하며 SaveArea 의 주소는 R13에 유지됩니다. 자세한 내용은  SaveArea 에서 확인한다.

 3. GPR 14 : 제어가 전달될 때 호출자의 반환 주소를 유지한다. 호출된 모듈이 처리가 완료되면 이 주소로 제어를 반환하기위해 사용된다. 

 4. GPR 15 : 호출된 모듈의 실행이 시작될 명령어 주소가 포함된다. 처리가 완료되어 반환될때 return code 가 포함된다. 호출된 프로그램이 정상 처리되면 0 값을 가진다.

### SaveArea
18개의 4바이트 영역으로 이루어져 있으며, 제어가 모듈에서 모듈로 전달 될 때 레지스터 내용을 유지하기 위해 사용된다.
호출된 모듈(Callee)에서는, 호출한 모듈(Caller)에서 사용하고 있던 레지스터 내의 데이터를 안전하게 저장할 수 있는 영역을 별도로 정의해야 한다.
아래는 이러한 개념을 간단하게 나타낸 그림이다.

![SaveArea]({{site.baseurl}}/attach/define_save_area.png)

Save Area는 아래와 같은 구조를 가진다.

![SaveAreaStructer]({{site.baseurl}}/attach/save_area_str.png)

* PSA(Previous Save Area)는 이전 SaveArea 의 주소를 가지고 있다.
* NSA(Next Save Area)는 다음 SaveArea 의 주소를 가지고 있다.
* R14는 호출자의 리턴 주소를 가지고 있다.
* R15는 호출된 프로그램의 진입점 주소를 가지고 있다.
* R0 – R12 는 레지스터 R0에서 R12의 내용을 포함한다.

각 모듈의 SaveArea는 자신을 호출한 모듈의 SaveArea의 주소를 위한 공간(PSA)을 가지고 있으며, 앞으로 자신이 호출할 모듈의 SaveArea에 대한 주소를 저장할 공간(NSA)도 남겨두고 있다. 이로 인해 실행 중인 프로그램이 double linked list 형태로 이어지게 된다. 따라서, 만약 프로그램 실행 중 에러가 발생하여 제어가 하나의 모듈에서 다른 모듈로 넘어갈 때 이러한 공간(PSA 혹은 NSA)에 접근하여 현재 레지스터를 결정할 수 있다.
아래 그림은 모듈간 SaveArea의 관계를 나타낸다.

![relation_savearea]]({{site.baseurl}}/attach/save_area_linked.png)

호출한 모듈의 레지스터 내용을 안전한 영역에 저장하기 위해서, 호출된 모듈에서는 일반적으로 아래와 같이 STM 를 사용한다. STM(Store Multiple) 명령어는 여러 개의 레지스터 내용을 지정한 위치에 저장한다. 이러한 명령어가 대부분의 프로그램의 처음에 오는 이유가 이 때문이다. 아래는 STM를 사용하는 예이다.

![example_stm]({{site.baseurl}}/attach/save_area_stm.png)

이러한 내용을 바탕으로 프로그램 간 호출과 Save Area 사용 예시를 보면,

![example_savearea]({{site.baseurl}}/attach/example_save_area.png)

노란색으로 하이라이트된 BASR 이라는 명령어로 SUBPGM 프로그램을 호출하고, 호출된 프로그램은 가장 마지막 라인의 BCR 명령어를 통해 리턴 한다. 
- STEP1, STEP4 : 호출 모듈의 R14~12를 R13의 시작주소에서 12바이트 이동한 R14 위치로 복사한다.
- STEP2, STEP5 : 호출 모듈의 R13의 주소를 SaveArea에서 4바이트 이동한 곳에 복사한다.
- STEP3, STEP6 : 현재 SaveArea의 주소를 R13에 복사한다. 
- STEP7, STEP9 : 현재 SaveArea에서 4바이트 이동한 곳에 저장된 호출 모듈의 R13 주소를 다시 R13에 복사한다.
- STEP8, STEP10 : 복구한 호출 모듈의 SaveArea에서 저장된 R14~12를 현재 프로그램에서 사용하고 있는 R14~12로 갱신한다.

위와 같이 SaveArea는 호출 모듈과 호출된 모듈 간에 각자의 Register를 저장 및 복구 하기 위해서 사용한다. 

### Parameter List
모듈 간 데이터를 전달하기 위해 Parameter list 를 사용한다. Parameter list란 호출 모듈에서 호출된 모듈로 전달되는 데이터, 즉 파라미터의 주소를 담은 리스트를 의미한다.
제어가 한 모듈에서 다른 모듈로 넘어갈 때, Parameter list 의 주소는 R1이 가지고 있어야 한다.
아래는 두 개의 파라미터를 포함한 파라미터 리스트의 예이다.

![example_parameter_list]({{site.baseurl}}/attach/parameter_list.png)

파라미터 전달 시 사용하는 종류는 일반적으로 아래와 같이 나눠진다.
- Fixed-Length Parameter List
고정 갯수의 파라미터를 전달하는 경우 전달할 데이터와 길이를 고려한다. 
- Variable Length Parameter List
임의 수의 파라미터를 허용하도록 설계한다. 따라서, Parameter list내에 저장된 파라미터 주소의 갯수는 가변적이다. 마지막 파라미터 주소는 최상단비트 가 1 이어야 한다.

![kinds_of_plist]({{site.baseurl}}/attach/fixed_var_parameter_list.png)

다음은 Fixed-Length Parameter List 의 예시 이다.
아래 그림에서 2개의 Packed decimal과 연산 결과를 담을 변수, 총 3개의 파라미터를 PARMLST 라는 Parameter list 형태 1번 레지스터에 PARMLST 의 주소를 가르키게 하고, ADDPD 를 호출한다. 

![example_fixed_plist1]({{site.baseurl}}/attach/example_fixed_parameter_list_1.png)

아래 그림에서 3,4,5번 레지스터에 Parameter list 의 세 개의 파라미터를 multiple load 를 한다. 5번 레지스터에 3번 레지스터와 4번 레지스터의 값을 차례로 더한다. 처리가 완료된 레지스터를 호출 모듈의 SaveArea 에 Restore 후 Return 한다.

![example_fixed_plist2]({{site.baseurl}}/attach/example_fixed_parameter_list_2.png)

다음은 Variable-Length Parameter List 의 예시 이다.
아래 그림에서 가변 갯수의 파라미터를 Parameter list 를 구성하고 마지막 파라미터의 주소에 High order bit 를 1로 셋팅하여 AADDPD 를 호출한다.

![example_var_plist1]({{site.baseurl}}/attach/example_var_parameter_list1.png)

아래 그림에서  LOOP 를 돌며 LTR 를 이용하여 음수값일 경우 (high order bit 가 1인 경우) 즉, 마지막 파라미터인지 여부를 체크한다. 마지막 파라미터가 아닌 경우 더한다.

![example_var_plist2]({{site.baseurl}}/attach/example_var_parameter_list2.png)
![example_var_plist3]({{site.baseurl}}/attach/example_var_parameter_list3.png)

아래 그림에서  마지막 파라미터인 경우 더하고 호출 모듈 의 SaveArea Register 영역을 Restore 하고 Return 한다.

![example_var_plist4]({{site.baseurl}}/attach/example_var_parameter_list4.png)




