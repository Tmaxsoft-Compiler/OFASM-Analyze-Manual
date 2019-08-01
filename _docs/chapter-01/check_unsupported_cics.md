---
title: 미지원 CICS 커맨드 확인
category: Chapter 01
order: 5
---

## CICS 커맨드 확인
우선, CICS에서 사용되는 자산인지 판단하기 위해서 자산에서 CICS 명령어를 사용하는지 확인한다. CICS 명령어는 아래와 같은 형태를 가지고 사용된다. 

<pre>
    EXEC CICS [명령어] [옵션1] [옵션2] ...
</pre>

위와 같은 명령어는 어셈블러 기계 명령어가 아니기 때문에 전처리를 통해 기계 명령어로 변환해주는 작업이 필요하다.

이것은 cicsprep이라는 OSC관련 라이브러리를 통해 이루어지며, OFASM에서는 EXEC CICS~ 구문에 해당 라이브러리에서 제공하는 전처리 기능을 사용한다.

CICS에서 사용되는 자산임이 확인된 경우에는 해당 자산을 컴파일 할 때, --enable-cics 옵션을 붙여서 컴파일 한다.

<pre>
    ofasm TEST.asm --enable-cics
</pre>

일부 CICS 명령어는 지원을 하지 않을 수도 있다. 그런 경우에는 아래와 같은 메세지가 출력된다.

<pre>
OFASMPP: INVALID SYNTAX FOUND IN CICS STATEMENT. IGNORING.
cics_stmt : EXEC CICS CHANGE PASSWORD(OLDPWD) NEWPASSWORD(NEWPWD)            USERID(SYSUSER);
</pre>

위와 같은 메세지가 출력되면,

1. OSC 담당자에게 cicsprep을 통한 전처리 기능을 요청한다.
2. OSC 담당자에게 해당 CICS 명령어에 대한 기능을 요청한다.