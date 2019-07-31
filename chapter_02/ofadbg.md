## 자산 내 디버깅 메세지 삽입 방법

OFASM 프로그램 실행 도중 레지스터와 특정 VM 메모리 영역을 확인하고 싶을 때가 있다. 이러한 경우를 위해 OFASM 에서는 OFADBG~ 키워드가 붙은 특별한 기계명령어를 제공한다. 현재는 레지스터의 값을 조사하기 위한 OFADBGREG와 메모리 영역 확인을 위한 OFADBGMEM이 있으며, OFADBGREG 같은 경우 피연산자에 따라 다양한 기능을 제공하기도 한다.

### OFADBGREG

<pre>
    OFADBGREG   I1
</pre>

I format. OFADBGREG 명령어의 기본적인 기능은 레지스터에 저장된 값을 stdout으로 출력하기 위함이다. 하지만 아래에 정리된 것과 같이, 피연산자에 따라 다양한 기능을 제공한다. 이러한 기능들은 보통 런타임 과정에서 생기는 문제들을 분석하기 위한 용도로 사용된다.

|피연산자   |기능       | 사용예    | 설명      |
| ---       | ---       | ---       | ---       |
| 0 ~ 15    | 0 ~ 15 번째 레지스터에 저장된 값을 출력한다.                          | OFADBGREG   4     | 레지스터 4번의 내용을 출력한다    |
| 100 ~ 115 | 뒤의 두자리 숫자에 대응하는 레지스터에 대해 Watch point를 설정한다.   | OFADBGREG    105  | 레지스터 5번에 대한 Watch point 설정 | 
| 200 ~ 209 | 이 명령어가 사용된 순간부터 OFASM VM이 종료될때까지 로그 레벨을 0 ~ 9로 설정한다  | OFADBGREG   205   | 로그 레벨을 5로 설정 |
| 255 | 강제로 Assert 함수를 호출한다.| OFADBGMEM   255 | |

//TODO: OFADBGREG 출력 예시 그림 파일 첨부

### OFADBGMEM

<pre>
    OFADBGMEM   D1(L1,B1),D2(L2,B2)
</pre>

SS format. OFADBGMEM 명령어는 OFASM VM 메모리 영역의 내용을 stdout으로 출력한다. 두 번째 피연산자 값은 사용자의 편의에 따라 0 ~ 3을 설정할 수 있으며, 이에 따라 다양한 포맷으로 메모리를 검사할 수 있다.

1. 두 번째 피연산자가 0일 때

<pre>
    OFADBGMEM   DATA0,0
</pre>

2. 두 번째 피연산자가 1일 때

<pre>
    OFADBGMEM   DATA1,1
</pre>

3. 두 번째 피연산자가 2일 때

<pre>
    OFADBGMEM   DATA2,2
</pre>

4. 두 번째 피연산자가 3일 때

<pre>
    OFADBGMEM   DATA3,3
</pre>

첫 번째 피연산자가 S포맷이므로 해당 피연산자에 리터럴 형태의 데이터를 넣는 것도 가능하다
<pre>
    OFADBGMEM   =C'LABEL1',2
</pre>

//TODO: OFADBGMEM 출력 예시 그림 파일 첨부