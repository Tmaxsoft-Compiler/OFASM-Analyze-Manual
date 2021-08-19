---
title: Assembler 구조
category: ASM Programming Introduction
order: 2
---

## 목차

- [목차](#목차)
- [Assembler Structure](#assembler-structure)
  - [Gerenal Purpose Register](#gerenal-purpose-register)
  - [SaveArea](#savearea)
  - [Parameter Lists](#parameter-lists)

----

## Assembler Structure
### Gerenal Purpose Register
Register는 0 ~ 15번 총 16개가 있으며, CPU가 접근할 수 있는 가장 빠른 저장 공간(메모리)를 말한다. 그 중의 하나로 General Purpose Register(GPR)가 있으며, 프로그래머가 Assembly 프로그램에서 사용할 수 있도록 제공되는 4바이트 길이 Register들을 말한다. 사용 목적은 데이터의 저장, 주소 지정, 제어권 전달 등을 보다 빠르게 처리하기 위함이다. 아래 레지스터 종류에 대한 그림이다. 주황색은 특수 목적 레지스터를 의미하며 파란색은 일반 목적 레지스트를 의미한다.

![레지스터]({{site.baseurl}}/attach/register.png)

64비트와 32비트의 모든 컴퓨터에서 지원하기 위해 각 Register들은 총 길이 8바이트로 지정됩니다. 가장 왼쪽 4바이트는 “0x00(null)”로 채워진다.

아래는 5개 레디스터는 모듈간 제어 및 데이터를 전달하고 반환 할때 사용된다.

1. GPR 0 : Register 0는 0 값을 의미하여 4096바이트 스토리지의 처음을 참조할수 있다. 아래 예제를 보면 R0의 값을 변경한 후에 R9에 어떤 값이 들어가는지를 확인해보면 명확해진다. 4번째 줄의 첫 LA문을 통해 R0에 들어간 값은 변경되지만, 다음 LA 시 R0 는 여전히 0으로 간주됨을 확인할 수 있다.

        MAINPROC EQU   *
                USING PARALIST,R7
                L     R7,0(R1)             R7 = R1의 첫 번째 인자의 주소가 저장
                LA    R0,2(R0,R7)          R0 = R7의 주소 + 0(Index) + 2(Displacement) = R7 + 2
                LA    R9,2(R0,R7)          R9 = R7의 주소 + 0(Index) + 2(Displacement) = R7 + R0(?) + 2

 1. GPR 1 : Register 1은 파라미터 리스트의 주소를 가지고 있다. 자세한 내용은 Parameter Lists 에서 확인한다.

 2. GPR 13 : SaveArea 주소를 유지하는 데 사용된다. 레지스터를 저장하는 데 사용되는 영역을 SaveArea 라고 하며 SaveArea 의 주소는 R13에 유지됩니다. 자세한 내용은  SaveArea 에서 확인한다.

 3. GPR 14 : 제어가 전달될 때 호출자의 반환 주소를 유지한다. 호출된 모듈이 처리가 완료되면 이 주소로 제어를 반환하기위해 사용된다. 

 4. GPR 15 : 호출된 모듈의 실행이 시작될 명령어 주소가 포함됩니다. 처리가 완료되어 반환될때 return code 가 포함된다. 이 값은 일반적으로 호출된 프로그램이 정상 처리되면 0 값을 가지고 있다.

### SaveArea
제어가 모듈에서 모듈로 전달 될때 레지스터 내용을 유지하기 위해 사용되는 4바이트 18개의 영역으로 이루어져 있다.
호출된 모듈에서는 호출한 모듈에 대한 레지스터를 안전하게 저장할 영역을 정의해야 한다. 아래 관련 그림이다.

![SaveArea]({{site.baseurl}}/attach/define_save_area.png)

Save Area 의 형태는 아래와 같다.

![SaveAreaStructer]({{site.baseurl}}/attach/save_area_str.png)

* PSA는 이전 SaveArea 의 주소를 가지고 있다.
* NSA는 다음 SaveArea 의 주소를 가지고 있다.
* R14는 호출자의 리턴 주소를 가지고 있다.
* R15는 호출된 프로그램의 진입점 주소를 가지고 있다.
* R0 – R12 는 레지스터 R0에서 R12의 내용을 포함한다.

호출되어 지는 모듈의 각 SaveArea 는 이전 SaveArea 와 다음 SaveArea 를 가리킵니다. 이로인해 실행중인 program 이 linked list 형태로 이어지게 됩니다. 만약 프로그램 실행 중 실패를 하게 되어 제어가 모듈에서 모듈로 넘어갈 때 각 지점에서 레지스터를 결정할 수 있다. 아래 그림은 모듈간 SaveArea의 관계를 나타낸다.

![relation_savearea]]({{site.baseurl}}/attach/save_area_linked.png)

위에서 호출된 모듈에서는 호출한 모듈에 대한 레지스터를 안전한 영역에 저장해야 하는데, 일반적인 방법으로 아래와 같이 STM 를 사용한다. 다중 저장(STM) 명령어는 레지스터의 내용을 올바른 위치에 저장한다. 이러한 명령어가 모든 프로그램의 처음에 오는 이유가 이 때문이다. 아래는 STM를 사용하는 예이다.

![example_stm]({{site.baseurl}}/attach/save_area_stm.png)

이러한 내용을 바탕으로 프로그램 간 호출과 Save Area 사용 예시를 보면,

![example_savearea]({{site.baseurl}}/attach/example_save_area.png)

노란색으로 하이라이트된 BASR 이라는 명령어로 SUBPGM 프로그램을 호출하고, 호출된 프로그램은 가장 마지막 라인의 BCR 명령어를 통해 리턴 한다. 
- STEP1, STEP4 : Caller의 R14~12를 R13의 시작주소에서 12byte 이동한 R14 위치로 복사한다.
- STEP2, STEP5 : Caller의 R13의 주소를 SaveArea에서 4byte 이동한 곳에 복사한다.
- STEP3, STEP6 : 현재 SaveArea의 주소를 R13에 복사한다. 
- STEP7, STEP9 : 현재 SaveArea에서 4바이트 이동한 곳에 저장된 Caller의 R13 주소를 다시 R13에 복사한다.
- STEP8, STEP10 : 복구한 Caller SaveArea에서 저장된 R14~12를 현재 프로그램에서 사용하고 있는 R14~12로 갱신한다.

위와 같이 SaveArea는 Caller <-> Callee 간 기존 Register를 저장 및 복구 하기 위해서 사용한다. 

### Parameter Lists
모듈간 데이터를 전달하는 데 Parameter list 를 사용한다. 제어가 한 모듈에서 다른 모듈로 넘어갈 때, Parameter list 의 주소는 R1에 있다. 매개변수들은 호출자에서 호출된 모듈로 전달되는 데이터의 주소 목록으로 정의된다.
Parameter list의 각 항목은 해당 데이터의 4바이트 주소를 포함한다. 아래는 두개의 파라미터를 포함한 파라미터의 예이다.

![example_parameter_list]({{site.baseurl}}/attach/parameter_list.png)

매개변수 전달시 일반적으로 사용하는 두가지 형태의 소개한다. 아래는 관련 항목과 그림이다.
- Fixed-Length Parameter List
고정 갯수의 매개변수를 전달하는 경우 전달할 데이터와 길이를 고려한다. 
- Variable Length Parameter List
임의 수의 매개변수를 허용하도록 설계한다. Parameter list의 길이가 가변적이다. 마지막 파라미터는 high order bit 가 1 이어야 한다.

![kinds_of_plist]({{site.baseurl}}/attach/fixed_var_parameter_list.png)

다음은 Fixed-Length Parameter List 의 예시 이다.
아래 그림에서 2개의 Packed decimal과 합을 결과를 담을 변수 총 3개의 매개변수를 PARMLST 라는 parameter list 형태를 만들어 1번 레지스터에 PARMLST 의 주소를 가르키게 하고, ADDPD 를 호출한다. 

![example_fixed_plist1]({{site.baseurl}}/attach/example_fixed_parameter_list_1.png)

아래 그림에서 3,4,5번 레지스터에 Parameter list 의 세개의 매개변수를 multiple load 를 한다. 5번 레지스터에 3번 레지스터와 4번 레지스터의 값을 차례로 더한다. 처리가 완료된 레지스터를 caller save area 에 Restore 후 return 한다.

![example_fixed_plist2]({{site.baseurl}}/attach/example_fixed_parameter_list_2.png)

다음은 Variable-Length Parameter List 의 예시 이다.
아래 그림에서 가변 갯수의 매개변수를 Parameter list 를 구성하고 마지막 매개변수에 High order bit 를 1로 셋팅하여 AADDPD 를 호출한다.

![example_var_plist1]({{site.baseurl}}/attach/example_var_parameter_list1.png)

아래 그림에서  LOOP 를 돌며 LTR 를 이용하여 음수값일 경우 (high order bit 가 1인 경우) 즉, 마지막 파라미터인지 여부를 체크한다. 마지막 파라미터가 아닌 경우 더한다.

![example_var_plist2]({{site.baseurl}}/attach/example_var_parameter_list2.png)
![example_var_plist3]({{site.baseurl}}/attach/example_var_parameter_list3.png)

아래 그림에서  마지막 파라미터인 경우 더하고 caller 의 SaveArea register 영역을 Restore 하고 return 한다.

![example_var_plist4]({{site.baseurl}}/attach/example_var_parameter_list4.png)




