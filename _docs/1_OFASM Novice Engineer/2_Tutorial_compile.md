---
title: 컴파일
category: OFASM Novice Engineer
order: 2
---

## 목차 
- [목차](#목차)
- [컴파일](#컴파일)
  - [컴파일 방법](#컴파일-방법)
  - [컴파일하여 즉시 실행](#컴파일하여-즉시-실행)

---

## 컴파일

### 컴파일 방법

1. HELLOASM.asm 이름으로 아래와 같이 샘플 코드를 작성한다.

        HELLOASM   CSECT
                  LR        12,15
                  USING     HELLOASM,12
        *
                  L         2,0(1)
                  MVC       PARAM,0(2)
                  OFADBGMEM GREETING,1
        *
                  BR        14
        GREETING  DS        0CL30
                  DC        CL15'Hello, This is '
        PARAM     DS        CL5
                  DC        CL10' Test Code'
                  END

2. 컴파일을 수행할 수 있다.

        $ ofasm HELLOASM.asm
        [OFASMPP: SUCC(0)] HELLOASM.asm
        ofasma: Program size of HELLOASM.asmo = 50 byte
        ofasma: assembly success


    > 소스 전처리만 수행 가능하다. 수행 후 HELLOASM.asmi 파일 생성 여부를 확인한다.

            $ ofasm HELLOASM.asm -E 
            [OFASMPP: SUCC(0)] HELLOASM.asm
            
            $ ls -l HELLOASM.asmi 
            -rw-rw-r-- 1 oframe7 oframe7 369 May 28 00:35 HELLOASM.asmi
    

    > 전처리 파일에 대해 어셈블리도 가능하다. 수행 후 HELLOASM.asmo 파일 생성 여부를 확인한다 
        
            $ ofasm HELLOASM.asmi -S 
            ofasma: Program size of HELLOASM.asmo = 50 byte
            ofasma: assembly success
            $ ls -l HELLOASM.asmo 
            -rw-rw-r-- 1 oframe7 oframe7 960 May 28 00:36 HELLOASM.asmo

### 컴파일하여 즉시 실행

1. [컴파일 방법](#컴파일-방법) 에서 작성한 HELLOASM.asm 를 -x 옵션으로 컴파일하여 즉시 실행한다.

        $ ofasm HELLOASM.asm -x "OFASM"
        [OFASMPP: SUCC(0)] HELLOASM.asm
        ofasma: Program size of HELLOASM.asmo = 50 byte
        ofasma: assembly success
        Length=[0000000000000030], Hex=[0x48656c6c6f2c2054686973206973204f4641534d205465737420436f6465], Char=[Hello, This is OFASM Test Code]
