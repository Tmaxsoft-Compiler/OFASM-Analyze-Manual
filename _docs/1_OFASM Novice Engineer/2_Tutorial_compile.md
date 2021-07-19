---
title: OFASM Tutorial Compile
category: OFASM Novice Engineer
order: 2
---

## 목차 
* 컴파일
  * 컴파일 방법
  * 컴파일하여 즉시 실행

---

## 컴파일

### 컴파일 방법

1. TESTASM2.asm 이름으로 아래와 같이 샘플 코드를 작성한다.

        TESTASM2  CSECT
                LR 12,15
                USING TESTASM2,12
        *
                L 2,0(1)
                MVC PARAM,0(2)
                OFADBGMEM GREETING,1
        *
                BR 14
        GREETING  DS 0CL30
                DC CL15'Hello, This is '
        PARAM     DS CL5
                DC CL10' Test Code'
                END

2. 컴파일을 수행할 수 있다.

        $ ofasm TESTASM2.asm
        ofasma: Program size of TESTASM.asmo = 31 byte
        ofasma: assembly success


    > 소스 전처리만 수행 가능하다. 수행 후 TESTASM2.asmi 파일 생성 여부를 확인한다.

            $ ofasm TESTASM2.asm -E 
            OFASMPP: SUCC(0)] TESTASM2.asm 
            
            $ ls -l TESTASM.asmi 
            -rw-rw-r-- 1 oframe7 oframe7 360 Jul 12 04:25 TESTASM2.asmi
    

    > 전처리 파일에 대해 어셈블리도 가능하다. 수행 후 TESTASM2.asmo 파일 생성 여부를 확인한다 
        
            $ ofasm TESTASM.asmi -S 
            ofasma: Program size of TESTASM.asmo = 31 byte 
            ofasma: assembly success 
            $ ls -l TESTASM2.asmo 
            -rw-rw-r-- 1 oframe7 oframe7 880 Jul 12 04:23 TESTASM2.asmo

### 컴파일하여 즉시 실행

1. _(컴파일 방법)_ 에서 작성한 TESTASM2.asm 를 -x 옵션으로 컴파일하여 즉시 실행한다.

        $ ofasm TESTASM2.asm -x "OFASM"

        ofasma: Program size of TESTASM2.asmo = 50 byte
        ofasma: assembly success
        Length=[0000000000000030], Hex=[0x48656c6c6f2c2054686973206973204f4641534d205465737420436f6465],
        Char=[Hello, This is OFASM Test Code]
