## Properties 설정

OFASM 설치 경로 내의 config/properties.conf 파일 내에 정의된 각 필드에 대한 설명 및 설정 방법에 대해 기술한다.

### OFASM_LOG

이 필드는 Debug 바이너리에 대해서만 적용된다.
OFASM은 OFASM_LOG 필드에 설정된 로그 레벨에 따라 어셈블러 프로그램 실행 정보를 출력한다.

실행환경 필드
* ONLINE : CICS 환경에서 출력하는 로그에 대해 설정한다
* BATCH: 배치 환경에서 출력하는 로그에 대해 설정한다.

실행환경 필드는 각각 아래의 필드를 가진다

* OFASM_LOG_LEVEL : 로그 레벨을 설정한다. 설정할 수 있는 값은 0~5이다.
* OFASM_LOG_AFTER : 몇 번째 Instruction 부터 로그를 출력할지를 결정한다. 0이면 출력하지 않는다.

### NETCOBOL_COMPATIBLE

ON 또는 OFF

Netcobol과 호환하여 OFASM을 실행할 때 ON으로 설정한다. 이 필드의 적용범위는 컴파일 과정도 포함하기 때문에, 이 필드를 ON으로 변경하였을 때에는 해당되는 자산들을 모두 재컴파일 하는 것이 좋다
