---
title: 디버그
category: Chapter 02
order: 3
---

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

#### 피연산자가 0 ~ 15일 때

<pre>
        OFADBGREG   2
</pre>

<pre>
Reg=[02], Hex=[0x0000000054455354], CC=[0]
</pre>

#### 피연산자가 100 ~ 115일 때

<pre>
        OFADBGREG   102
</pre>

<pre>
OFASM: OFADBGREG[102], ADD WATCH REGISTER [2]
OFASM: WATCHING REGISTER = R2
REG = [2]
OLD=[0x0000000000000000]
NEW=[0x0000000054455354]
</pre>

#### 피연산자가 200 ~ 209일 때

<pre>
        OFADBGREG   205
</pre>

<pre>
OFASM: OFADBGREG[205], SET OFASM_LOG_LEVEL[5]
[00000006] 128460  ==>  LA      2,32(0,12)
...
</pre>

### OFADBGMEM

<pre>
    OFADBGMEM   D1(L1,B1),D2(L2,B2)
</pre>

SS format. OFADBGMEM 명령어는 OFASM VM 메모리 영역의 내용을 stdout으로 출력한다. 두 번째 피연산자 값은 사용자의 편의에 따라 0 ~ 3을 설정할 수 있으며, 이에 따라 다양한 포맷으로 메모리를 검사할 수 있다.

#### 두 번째 피연산자가 0일 때

* 대상 심볼이 가리키는 데이터의 바이트 단위 길이
* 대상 심볼의 Hex 값

소스 코드
<pre>
        OFADBGMEM   DATA1,0
        OFADBGMEM   DATA2,0
        OFADBGMEM   DATA3,0
...
DATA1   DC      C'TEST'
DATA2   DC      P'+25594'
DATA3   DC      Z'+1234'
</pre>

출력
<pre>
Length=[0000000000000004], Hex=[0x54455354]
Length=[0000000000000003], Hex=[0x25594c]
Length=[0000000000000004], Hex=[0x31323334]
</pre>

#### 두 번째 피연산자가 1일 때

* 대상 심볼이 가리키는 데이터의 바이트 단위 길이
* 대상 심볼의 데이터 (Hex 값)
* 대상 심볼의 데이터 (문자열)

<pre>
        OFADBGMEM   DATA1,1
        OFADBGMEM   DATA2,1
        OFADBGMEM   DATA3,1
</pre>

<pre>
Length=[0000000000000004], Hex=[0x54455354], Char=[TEST]
Length=[0000000000000003], Hex=[0x25594c], Char=[%YL]
Length=[0000000000000004], Hex=[0x31323334], Char=[1234]
</pre>

#### 두 번째 피연산자가 2일 때

* 대상 심볼의 Location counter 위치(VM 내 주소값)
* 대상 심볼이 가리키는 데이터의 바이트 단위 길이
* 대상 심볼의 데이터 (Hex 값)
* 대상 심볼의 데이터 (문자열)

<pre>
        OFADBGMEM   DATA1,2
        OFADBGMEM   DATA2,2
        OFADBGMEM   DATA3,2
</pre>

<pre>
Addr=[0000000000128524], Length=[0000000000000004], Hex=[0x54455354], Char=[TEST]
Addr=[0000000000128528], Length=[0000000000000003], Hex=[0x25594c], Char=[%YL]
Addr=[0000000000128531], Length=[0000000000000004], Hex=[0x31323334], Char=[1234]
</pre>

#### 두 번째 피연산자가 3일 때

* 대상 심볼을 포함하는 프로그램의 이름
* 대상 심볼의 심볼 이름
* 대상 심볼이 가리키는 데이터의 바이트 단위 길이
* 대상 심볼의 데이터 (Hex 값)

<pre>
        OFADBGMEM   DATA1,3
        OFADBGMEM   DATA2,3
        OFADBGMEM   DATA3,3
</pre>

<pre>
Program=[TEST              ], Symbol=[DATA1             ], Length=[0000000000000004], Hex=[0x54455354]
Program=[TEST              ], Symbol=[DATA2             ], Length=[0000000000000003], Hex=[0x25594c]
Program=[TEST              ], Symbol=[DATA3             ], Length=[0000000000000004], Hex=[0x31323334]
</pre>

#### 두 번째 피연산자가 리터럴 값일 때

* 해당 심볼에 대한 Watch 기능을 활성화한다. 
* 두 번째 피연산자에 위치한 리터럴 값은 정수값(F-type)이어야 하며, 해당 정수값의 길이만큼을 Watch 대상으로 지정한다.

<pre>
        OFADBGMEM   DATA1,=F'4'
...
        MVC     DATA1,=C'ABCD'
...
DATA1   DC      C'TEST'
</pre>

<pre>
* Watch 옵션을 활성화한 OFADBGMEM 명령어를 VM이 실행하는 순간부터 아래와 같은 메세지가 출력된다.
Addr=[0000000000128464], OFASM: WATCHING LOCATION:LENGTH  = 128464:4
Length=[0000000000000004], Hex=[0x54455354]
Addr = [128464], SIZE = [4]
OLD=[0x00000000]
NEW=[0x54455354]
...
...
* 이후 Watch point에 등록 데이터가 변경되면 아래와 같은 메세지가 출력된다.
Addr = [128464], SIZE = [4]
OLD=[0x54455354]
NEW=[0x41424344]
</pre>

첫 번째 피연산자가 S포맷이므로, 리터럴 형태의 데이터를 넣는 것도 가능하다
<pre>
        OFADBGMEM   =C'LABEL1',2
</pre>

<pre>
Addr=[0000000000128542], Length=[0000000000000006], Hex=[0x4c4142454c31], Char=[LABEL1]
</pre>