---
title: 시스템 매크로 확인
category: Chapter 01
order: 4
---

## 시스템 매크로 확인

Host에서는 z/OS를 사용하며, z/OS에서 사용되는 시스템 매크로는 MVS38 이라는 오래된 운영체제에서 사용되던 매크로를 확장하여 사용하고 있다. MVS38에서 사용되는 매크로는 현재 소스가 공개되어 그대로 사용해도 문제가 되지 않기 때문에, OFASM에서는 이것을 이용하여 시스템 매크로를 지원하고 있다. 다만 MVS38 매크로는 앞서 언급한바와 같이 z/OS의 확장이므로 z/OS의 모든 사양을 지원하지 않으며, 또한 OFASM도 MVS38 매크로의 모든 사양을 지원하지 않는다. 일부는 OFASM 특성 상 동작이 필요하지 않은 것도 있으며, 또다른 일부는 OpenFrame 특성 상 지원하지 못하는 것도 있다. 

MVS38 매크로 중 일부 z/OS 사양 지원을 위해 OFASM이 따로 제공하는 매크로 파일은 아래와 같다(ofmac 디렉토리를 통해 제공). 

|   매크로이름  |   비고    |
|   ---         |   ---     |
|   CVT         |           |
|   GET         |시스템 함수 호출 주소 변경|
|   GETMAIN     |           |
|   IDACB2      |SHOWCB 에러 처리 지원|
|   IEZDEB      |일부 상수 타입 지원|
|   PUT         |시스템 함수 호출 주소 변경|
|   TIME        |&DATETYPE=YYYYDDD,MMDDYYYY,DDMMYYYY,YYYYMMDD 지원|
|   WTO         |&TEXT 지원 |
|   WTOR        |&TEXT 지원 |

### 지원 중인 시스템 매크로


### 미지원/일부지원 시스템 매크로

현재 OpenFrame 및 OFASM에서 지원하지 않는(혹은 지원할 수 없는) 매크로.

|   매크로이름      |   비고                                |
|   ---             |   ---                                 |
| ATTACH, DETACH    | 프로세스 기반인 OFASM에서 지원 불가   |
| STAE, ESTAE       | Exit 프로그램 등록 기능 없음          |
| STIMERM           | TIMEOUT 후에 OFASM 프로그램으로 리턴하는 기능 없음. 지원 불가 |
| SPIE, ESPIE       | 일부 기능 지원 불가                   |
| DYNALLOC          | ALLOC, DEALLOC 기능만 지원            |

현재 OFASM에서 미지원하는 매크로 파라미터

|   매크로이름      |   파라미터 | 비고                     |
|       ---         |    ---     |          ---             |
|   TIME            | TU         |                          |
|                   | BIN        |                          |
|                   | MIC        |                          |
|                   | STCK       |                          |
|                   | STCKE      |                          |
|                   | ,ZONE=UTC  |                          |
|                   | ,ZONE=GMT  |                          |
