---
title: Assembler 데이터셋
category: ASM Programming Introduction
order: 5
---

## 목차

- [목차](#목차)
- [Assembler Dataset](#assembler-dataset)
  - [Data Control Block Definition (DCB)](#data-control-block-definition-dcb)
  - [DCB kinds of parameters](#dcb-kinds-of-parameters)
  - [Dataset i/o macro](#dataset-io-macro)
  - [Vsam control macro](#vsam-control-macro)
    - [Generate an access method control block (ACB)](#generate-an-access-method-control-block-acb)
      - [Parameters for ACB](#parameters-for-acb)
    - [Create a request parameter list (RPL)](#create-a-request-parameter-list-rpl)
      - [Parameters for RPL](#parameters-for-rpl)
    - [Generate a control block (GENCB)](#generate-a-control-block-gencb)
      - [Parameters for GENCB](#parameters-for-gencb)
  
----

## Assembler Dataset
### Data Control Block Definition (DCB)
데이터 제어 블록(DCB)이라고 하는 파일 정의는 어셈블러 프로그래머가 프로그램에서 액세스하려는 각 파일에 대해 생성되어야 한다. DCB는 외부 파일에 대한 정보를 유지 관리하는 특별한 종류의 데이터 영역이다. DCB는 시스템 매크로 명령을 사용하여 생성된다.

DCB에는 다음이 포함된다.
* 데이터를 참조하는 데 사용되는 이름
* 데이터세트 내 레코드 및 블록의 형식 및 크기
* 데이터셋에 접근하기 위한 명령어의 종류와 데이터셋의 현재 상태

대부분의 어셈블러 프로그램의 주요 목적은 데이터(INPUT)를 읽고 처리한 다음 결과 데이터(OUTPUT)를 인쇄하는 것이다. 프로그램에서 입력 및 출력 데이터를 처리하는 주요 단계는 아래와 같다. I/O 매크로는 어셈블러 명령어로 수행되는 I/O 영역의 정의를 제외하고 이러한 모든 작업을 수행하는 데 사용된다. 아래 관련 그림이다.

![dataset_input_output_macro]({{site.baseurl}}/attach/dataset_input_output_macro.png)

I/O를 처리하는 모든 어셈블러 프로그램은 처리할 데이터셋에 대한 정보를 어셈블러에 제공해야 한다. 데이터는 순차적으로 구성되고 동일한 방식으로 DCB(Data Control Block) 매크로에 의해 처리된다. DCB 매크로 명령어는 데이터셋을 처리하기 위해 큰 블록의 상수를 생성한다. 상수 블록은 처리 중인 데이터셋의 레이아웃을 제어하는 데이터 필드(또는 파라미터) 세트이다.

DCB는 다음을 포함하여 데이터셋에 관한 정보를 포함하는 데이터 필드 모음이다. 아래 관련 항목과 그림이다.
* 레코드의 크기와 형식
* 데이터 처리에 사용되는 I/O 매크로
* 데이터셋의 이름 및 현재 상태

![dcb_data_field]({{site.baseurl}}/attach/dcb_data_field.png)

### DCB kinds of parameters
다양한 키워드 파라미터가 입력 및 출력 DCB와 함께 사용된다. DDNAME 파라미터는 이 DCB가 나타내는 외부 파일에 대한 링크로 사용된다. DDNAME 파라미터의 값은 일괄 실행을 위한 JCL DD 문의 DDNAME 또는 대화식 실행을 위한 ALLOCATE TSO 문의 FILE 파라미터와 일치해야 한다. 아래 관련 그림이다.

![dcb_ddname_using]({{site.baseurl}}/attach/dcb_ddname_using.png)

DCB macro 의 파라미터로 아래와 같은 종류가 올 수 있다 :
* DSORG 파라미터는 이 DCB가 나타내는 데이터셋의 구성을 지정한다. 이 과정의 예에서 파라미터는 Physical Sequential의 약어인 PS를 사용하고 있다. 
* MACRF 파라미터는 데이터셋의 레코드에 액세스하는 데 사용되는 매크로 형식을 지정한다. 입력에 대해 GM(GET 매크로 모드)을 지정하고 출력에 대해 PM(PUT 매크로 모드)을 지정한다.
* RECFM 파라미터는 처리 중인 데이터셋의 레코드 형식을 지정한다. 입력에 대해 FB(고정 길이 레코드)를 지정하고 출력에 FBA(미국 국립 표준 연구소(ANSI) 프린터 제어 문자가 있는 고정 길이 레코드)를 지정한다.
* LRECL 파라미터는 데이터셋의 레코드 길이를 지정한다. 위의 예에서는 입력에 대해 80을 지정하고 출력에 대해 133을 지정한다.
* BLKSIZE 파라미터는 데이터셋에서 물리적 레코드 또는 블록의 크기를 지정한다. 위의 예에서는 입력 데이터셋에 대해 이 파라미터를 생략하고 출력에 6650을 사용하고 있다.
* EODAD 파라미터(데이터 주소 끝)는 GET이 시도되고 더 이상 데이터가 없을 때 제어가 전송될 프로그램의 라벨을 지정한다. 이 파라미터는 입력 데이터셋에만 지정된다.

![dcb_parameter]({{site.baseurl}}/attach/dcb_parameter.png)

### Dataset i/o macro
* OPEN macro <br>
  데이터셋에 대한 입력 및 출력 작업을 수행하려면 먼저 데이터셋을 열거나 처리할 수 있어야 한다. 이를 위해 OPEN 매크로 명령어를 사용한다. OPEN 매크로는 운영 체제 루틴을 호출하여 데이터셋이 있는지 확인하고 사양과 일치하는지 확인하고 처리를 위해 준비하는 실행 가능한 명령을 생성한다. 출력 데이터셋의 경우 JCL 또는 ALLOCATE 문에서 지정하면 OPEN은 새 데이터셋을 생성한다. 아래 관련 그림이다.
  
  ![open_macro]({{site.baseurl}}/attach/open_macro.png)

* CLOSE macro <br>
  DCB가 open 되면 코드 데이터 전송 매크로, 입력용 GET 및 출력용 PUT. 이러한 매크로에 의해 I/O 처리가 끝나면 프로그램을 종료하기 전에 DCB를 닫는다. CLOSE 매크로는 해당 파일에 대한 파일 관련 처리를 마치고 더이상 사용하지 않고 싶을 때 사용한다. 아래 관련 그림이다.
  
  ![close_macro]({{site.baseurl}}/attach/close_macro.png)

* GET macro <br>
  데이터셋이 open 되면 GET 매크로는 데이터셋의 다음 논리 레코드를 작업 영역으로 읽기 작업을 수행한다. 입력 데이터의 끝에 도달하고 다른 GET 매크로가 실행되면 제어는 DCB 매크로에서 EODAD로 지정된 주소로 전송된다. 아래 관련 그림이다.
  
  ![get_macro]({{site.baseurl}}/attach/get_macro.png)

* PUT macro <br>
  PUT 매크로는 작업 영역에서 출력 데이터셋로 다음 논리 레코드에 쓰기 작업을 수행한다. 출력 모드에서 PUT 매크로의 형식은 아래와 같이 표시된다.

  ![put_macro]({{site.baseurl}}/attach/put_macro.png)

### Vsam control macro
VSAM을 사용하여 데이터를 구성하고 카탈로그에서 해당 데이터에 대한 정보를 유지 관리 및 액세스 메소드 서비스 명령 및 VSAM 매크로를 사용하여 VSAM 프로그래밍을 수행한다.
#### Generate an access method control block (ACB)
ACB 매크로 하위 파라미터의 값은 절대 숫자 표현식, 문자열, 코드 및 재배치 가능한 유효한 A 타입 주소 상수를 생성하는 표현식으로 지정할 수 있다.

VTAM을 사용하는 각 응용 프로그램은 ACB를 정의하고 열어야 한다. 응용 프로그램은 둘 이상의 ACB를 포함할 수 있지만, 각 ACB는 다른 응용 프로그램 이름을 나타내야 한다.

ACB가 열린 후에는 세션 설정 요청, 통신 작업 요청, 네트워크 관리 요청과 같은 VTAM 서비스에 대한 요청이 수행될 수 있다. ACB가 닫히면(CLOSE 매크로 명령 사용) 이러한 요청은 더 이상 수행될 수 없으며 설정된 세션이 종료된다.

ACB를 사용하여 응용 프로그램은 종료 루틴 주소 목록의 주소를 제공할 수 있다. 이 목록에 표시된 루틴은 오류 조건 또는 세션 설정 요청과 같은 특수 이벤트가 발생할 때 VTAM에 의해 호출된다.

ACB는 GENCB 매크로 명령을 사용하여 프로그램 실행 중에도 빌드할 수 있다. ACB는 MODCB 매크로 명령을 사용하여 프로그램을 실행하는 동안 또는 IFGACB 매핑 매크로 명령에 의해 생성된 DSECT를 사용하여 수정되지만 ACB가 열리기 전에만 수정된다.

OPEN 및 CLOSE 매크로 명령이 참조하는 ACB 제어 블록은 31비트 또는 24비트 저장소에 있을 수 있지만 응용 프로그램의 주소 지정 모드와 일치해야 한다. MODE 파라미터는 이러한 매크로 명령에 대한 ACB 제어 블록의 주소 지정 모드를 설정하는 데 사용된다.

##### Parameters for ACB
각 파라미터들에 대한 자세한 설명은 [ACB 매크로 스펙](https://www.ibm.com/docs/en/zos/2.1.0?topic=vmde-acb-generate-access-method-control-block-assembly-time#x4b__f9)를 참고 바란다. 
  
#### Create a request parameter list (RPL) 
응용 프로그램이 세션 설정 또는 통신을 위해 만드는 모든 요청은 요청 파라미터 목록(RPL)을 참조해야 한다.

응용 프로그램은 RPL을 사용하여 VTAM에 대한 대부분의 요청을 설명한다. 예를 들어 응용 프로그램은 RECEIVE를 발행하고 RPL을 표시할 수 있다. RPL은 VTAM에서 입력을 얻을 세션, 입력 데이터를 배치할 위치, 작업이 완료된 후 응용 프로그램에 알리는 방법 및 요청 처리 중에 따라야 하는 기타 옵션을 보여준다.

RPL 매크로 명령어는 어셈블리 중에 RPL을 빌드한다. 또한 GENCB 매크로 명령어는 프로그램 실행 중에 RPL을 생성할 수 있다. RPL 수정 요청은 RPL 기반 요청의 일부로 또는 MODCB 매크로 명령에 의해 수행될 수 있다. 어느 쪽이든 RPL 필드의 이름을 지정하고 새 값을 지정해야 한다. 또한 IFGRPL DSECT는 RPL 필드 값을 변경할 수 있다.

모든 RPL 피연산자는 선택적이며(AM=VTAM 제외) RPL 기반 매크로 명령어 중 하나로 지정할 수 있지만 각 RPL 기반 매크로 명령어는 매크로 명령어가 실행될 때 특정 RPL 필드를 설정해야 한다.

##### Parameters for RPL
각 파라미터들에 대한 자세한 설명은 [RPL 매크로 스펙](https://www.ibm.com/docs/en/zos/2.1.0?topic=examples-rpl-generate-request-parameter-list-assembly-time)에서 확인 바란다.

#### Generate a control block (GENCB)
GENCB 매크로 명령어는 ACB, EXLST, RPL 종류의 블록을 생성할 수 있다. GENCB 매크로 명령어 사용의 장점은 프로그램 실행 중에 제어 블록이 생성된다는 것이다. GENCB는 프로그램 실행 중에 제어 블록을 구축할 뿐만 아니라 동적으로 할당된 저장소에 제어 블록을 구축할 수도 있다. 해당 MACRO 운용의 한 가지 장점은 각 제어 블록의 길이에 대한 응용 프로그램 종속성을 제거할 수 있다는 것이다.

응용 프로그램의 메모리에 제어 블록을 구축하려면 응용 프로그램이 프로그램 어셈블리 중에 제어 블록을 수용할 만큼 충분한 메모리를 확보하거나 필요한 메모리를 확보하기 위해 운영 체제 메모리 조작 매크로 명령(GETMAIN)을 실행해야한다. 운영 체제 매크로 명령이 사용되는 경우 이 메모리의 위치와 길이는 GENCB 매크로 명령에 코딩되어야 한다. 위치 및 길이 피연산자(WAREA 및 LENGTH)가 생략되면 제어 블록에 대한 동적 메모리 할당이 자동으로 발생한다. 아래 관련 그림이다.

![gencb_macro]({{site.baseurl}}/attach/gencb_macro.png)

##### Parameters for GENCB
GENCB와 관련된 파라미터들에 대한 설명은 아래를 참고 바란다.
* [GENCB 매크로 스펙 - ACB](https://www.ibm.com/docs/en/zos/2.1.0?topic=vmde-gencb-generate-access-method-control-block-execution-time)
* [GENCB 매크로 스펙 - EXLST](https://www.ibm.com/docs/en/zos/2.1.0?topic=examples-gencb-generate-exit-list-execution-time)
* [GENCB 매크로 스펙 - RPL](https://www.ibm.com/docs/en/zos/2.1.0?topic=examples-gencb-generate-request-parameter-list-execution-time)