---
title: Configure 파일 설정 방법
category: Chapter 01
order: 3
---

## Configure 파일 설정 방법

### properties.conf 설정

* 컴파일타임에 적용되는 옵션에 대해서는 옵션 변경 이후 모든 어셈블러 자산을 다시 컴파일 한 후 실행해야 올바로 동작한다.

* **경로**: $OFASM_HOME/config/properties.conf

* **OFASM_LOG** : OFASM 프로그램이 실행하는 중에 stdout으로 출력되는 로그에 대한 옵션을 설정할 수 있다. 이 필드 및 이 필드가 포함하는 하위 필드에 대한 설정은 Debug 바이너리에만 적용된다.
    * **ONLINE** : ONLINE 제품에 대해 적용할 로그 옵션을 설정한다.
    * **BATCH** : BATCH 제품에 대해 적용할 로그 옵션을 설정한다.
        * **OFASM_LOG_LEVEL** : INTEGER. 로그 레벨을 설정한다.
          * 0: 기본값. 로그 비활성화
          * 1 ~ 5: 로그 활성화. 5가 최상위 수준을 나타낸다.
        * **OFASM_LOG_AFTER** : INTEGER. 로그를 출력할 때, 몇 번째 로그부터 출력할지를 결정한다.
          * 0: 기본값. 로그 비활성화
          * 1+ : 무시할 로그의 개수를 설정한다.
    * 적용범위: 런타임
* **NETCOBOL_COMPATIBLE** : ON/OFF. Netcobol 제품과 OFASM을 함께 사용할 때 이 필드를 ON 하여 사용한다.
    * **OFF** : 기본값. Netcobol 호환이 적용되지 않는다.
    * **ON** : Netcobol 호환을 적용한다. 
    * 적용범위: 런타임, 컴파일타임
* **VM_MEMORY_SIZE** : INTEGER. OFASM VM이 사용할 메모리의 크기를 메가바이트(MB) 단위로 설정한다. 최소값은 32이며, 최대값은 2048이다.
    * **0** : 기본값. 본 기본값을 사용할 때에 OFASM VM은 1024 MB (1 GB)의 메모리를 사용한다.
    * **32 ~ 2048** : OFASM 이 설정한 값만큼의 메모리를 메가바이트(MB) 단위로 사용한다.
    * 적용범위: 런타임