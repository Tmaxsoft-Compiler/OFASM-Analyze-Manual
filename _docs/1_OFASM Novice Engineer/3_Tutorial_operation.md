---
title: OFASM Tutorial Operation
category: OFASM Novice Engineer
order: 3
---

## 목차 
* ASM 호출 및 파라미터 운용
  * JCL 에서 호출되는 ASM
  * COBOL 에서 호출되는 ASM
    * Fixed parameter 로 전달
    * variable parameter 로 전달
    * pointer parameter 로 전달
  * ASM 에서 호출되는 COBOL
    * fixed parameter 로 전달
    * pointer parameter 로 전달
* Dataset 운용
  * NonVsam dataset I/O
  * Vsam dataset I/O

---

## ASM 호출 및 파라미터 운용

### JCL 에서 호출되는 ASM

* JCL PARM 키워드로 파라미터 전달 시 가변 길이 파라미터 형식의 interface 를 작성한다.

* JCL로 부터 전달받은 인자로 사칙연산을 수행하는 프로그램 작성

1. TESTASM3.asm 이름으로 아래 asm 소스 작성

        TESTASM3     CSECT
                    LR 12,15
                    USING TESTASM3,12
        *
                    L     2,0(1)
                    USING ARITHMETIC,2 
                    PACK  OP1DEC,OPERAND1
                    PACK  OP2DEC,OPERAND2
        *
                    CLI    OPERATOR,C'+'
                    BE    ADD
                    CLI    OPERATOR,C'-'
                    BE    SUBTRACT
                    CLI    OPERATOR,C'*'
                    BE    MULTIPLY
                    CLI    OPERATOR,C'/'
                    BE    DIVIDE
        *
        ADD          EQU   *
                    AP    OP1DEC,OP2DEC
                    B     ENDPGM
        *
        SUBTRACT     EQU   *
                    SP    OP1DEC,OP2DEC
                    B     ENDPGM
        *
        MULTIPLY     EQU   *
                    MP    OP1DEC,OP2DEC
                    B     ENDPGM
        *
        DIVIDE       EQU   *
                    DP    OP1DEC,OP2DEC
                    B     ENDPGM
        *
        ENDPGM       EQU   *
                    UNPK  RESULT,OP1DEC
                    OFADBGMEM RESULTMSG,1
                    SR    15,15
                    BR    14
        RESULTMSG    DS 0CL14
                    DC CL10'Answer is '
        RESULT       DS ZL4
        *
        OP1DEC       DS PL2
        OP2DEC       DS PL2
        *
        ARITHMETIC   DSECT
                    DS CL2
        OPERAND1     DS ZL2
        OPERATOR     DS CL1
        OPERAND2     DS ZL2
                    END


2. TESTJCL 이름으로 JCL 작성, 이때 PARM 키워드로 전달할 식을 string 으로 보낸다.

        //TESTASM3 JOB
        //JOBLIB   DD DSN=SYS1.USERLIB,DISP=SHR
        //STEP00   EXEC PGM=TESTASM3,PARM=(33+10)
        //SYSOUT   DD SYSOUT=*
        //


3. Entry interface 를 아래와 같이 TESTASM3.json 이름으로 작성한다.

        {
        "entry_list" : [
        {
            "entry_name" : "TESTASM3",
            "fixed_parameter_list" : [
            {
                "param_type" : "V"
            }
            ]
        }
        ],
        "program_name" : "TESTASM3",
        "version" : 3
        }

    > 전달할 파라미터가 없을 경우 아래 빈 fixed parameter list 를 작성한다.

        {
        "entry_list" : [
        {
            "entry_name" : "TESTASM3",
            "fixed_parameter_list" : []
        }
        ],
        "program_name" : "TESTASM3",
        "version" : 3
        }


4. 컴파일 및 디플로이

        $ ofasm TESTASM3.asm
        ofasma: Program size of TESTASM3.asmo = 196 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM3.json
        1 = -i
        2 = TESTASM3.json
        TESTASM3_OFASM_VM_ENTRY.cpp Interface generation complete

        $ g++ -shared -fPIC -o TESTASM3.so TESTASM3_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ dlupdate /home/oframe7/test/TESTASM3.so SYS1.USERLIB
        $ dlupdate /home/oframe7/test/TESTASM3.asmo SYS1.USERLIB


5. JCL 수행

        $ tjesmgr r /home/oframe7/test/TESTJCL
        Input USERNAME  : ROOT
        >
        Command : [r /home/oframe7/test/TESTJCL]
        Node name :  A N Y
        (JOB00010) /home/oframe7/test/TESTJCL is submitted as TESTASM3(JOB00010).


6. spool 확인

        $ tjesmgr
        > psj JOB00010
        > podd @j di=6
        > Length=[0000000000000014], Hex=[0x416e737765722069732030333330], Char=[Answer is 0330]


----------------------------------------------------------------

### COBOL 에서 호출되는 ASM

#### Fixed parameter 로 전달

* 고정된 길이와 정해진 수의 파라미터를 전달 받는 경우 Fixed parameter list 형태의 interface 사용한다.

* Cobol로 부터 전달은 식으로 사칙연산을 수행하는 asm 프로그램을 작성한다.

1. TESTASM4.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM4     CSECT
                    LR 12,15
                    USING TESTASM4,12
                    L     2,0(1)
                    L     2,0(2)
                    L     5,4(1)
                    MVC   OPERATOR,0(5)
                    L     6,8(1)
                    MVC   OPERAND2,0(6)
        *
                    CLI   OPERATOR,C'+'
                    BE    ADD
                    CLI   OPERATOR,C'-'
                    BE    SUBTRACT
                    CLI   OPERATOR,C'*'
                    BE    MULTIPLY
                    CLI   OPERATOR,C'/'
                    BE    DIVIDE
        *
        ADD          EQU   *
                    A     2,OPERAND2
                    B     ENDPGM
        *
        SUBTRACT     EQU   *
                    S     2,OPERAND2
                    B     ENDPGM
        *
        MULTIPLY     EQU   *
                    LA    3,0(2)
                    M     2,OPERAND2
                    LA    2,0(3)
                    B     ENDPGM
        *
        DIVIDE       EQU   *
                    LA    3,0(2)
                    D     2,OPERAND2
                    LA    2,0(3)
                    B     ENDPGM
        *
        ENDPGM       EQU   *
                    CVD   2,OPERAND1
                    UNPK  RESULT,OPERAND1
                    OFADBGMEM RESULTMSG,1
                    SR    15,15
                    BR    14
        RESULTMSG    DS 0CL14
                    DC CL10'Answer is '
        RESULT       DS ZL4
        *
        OPERAND1     DS PL8
        OPERATOR     DS CL1
        OPERAND2     DS FL4
                    END


2. TESTCOB1.cob 이름으로 아래와 같이 cobol 코드를 작성한다.

              ID DIVISION.
              PROGRAM-ID. TESTCOB1.
              DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 EXP.
                  03 OPERAND1 PIC S9(8) COMP.
                  03 OPERATOR PIC X.
                  03 OPERAND2 PIC S9(8) COMP.
              PROCEDURE DIVISION.

                  MOVE +20 TO OPERAND1.
                  MOVE '+' TO OPERATOR.
                  MOVE -10 TO OPERAND2.

                  CALL 'TESTASM4' USING OPERAND1
                                        OPERATOR
                                        OPERAND2.

                  MOVE +20 TO OPERAND1.
                  MOVE '-' TO OPERATOR.
                  MOVE -10 TO OPERAND2.

                  CALL 'TESTASM4' USING OPERAND1
                                        OPERATOR
                                        OPERAND2.

                  MOVE +10 TO OPERAND1.
                  MOVE '*' TO OPERATOR.
                  MOVE +20 TO OPERAND2.

                  CALL 'TESTASM4' USING OPERAND1
                                        OPERATOR
                                        OPERAND2.

                  MOVE +20 TO OPERAND1.
                  MOVE '/' TO OPERATOR.
                  MOVE +10 TO OPERAND2.

                  CALL 'TESTASM4' USING OPERAND1
                                        OPERATOR
                                        OPERAND2.



3. Entry interface 를 아래와 같이 TESTASM4.json 이름으로 작성한다.

        {
        "entry_list" : [
        {
            "entry_name" : "TESTASM4",
            "fixed_parameter_list" : [
            {
                "param_size" : 4,
                "param_type" : "NP"
            },
            {
                "param_size" : 1,
                "param_type" : "NP"
            },
            {
                "param_size" : 4,
                "param_type" : "NP"
            }
            ]
        }
        ],
        "program_name" : "TESTASM4",
        "version" : 3
        }


4. 컴파일 및 디플로이

        $ ofasm TESTASM4.asm
        [OFASMPP: SUCC(0)] TESTASM4.asm
        ofasma: Program size of TESTASM4.asmo = 157 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM4.json
        1 = -i
        2 = TESTASM4.json
        TESTASM4_OFASM_VM_ENTRY.cpp Interface generation complet

        $ g++ -shared -fPIC -o TESTASM4.so TESTASM4_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ ofcob -x TESTCOB1.cob


5. 실행 및 결과 확인

        $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.

        $ ./TESTCOB1
        Length=[0000000000000014], Hex=[0x416e737765722069732030303130], Char=[Answer is 0010]
        Length=[0000000000000014], Hex=[0x416e737765722069732030303330], Char=[Answer is 0030]
        Length=[0000000000000014], Hex=[0x416e737765722069732030323030], Char=[Answer is 0200]
        Length=[0000000000000014], Hex=[0x416e737765722069732030303032], Char=[Answer is 0002]


#### Variable parameter 로 전달

* 가변 길이의 정해진 수가 아닌 파라미터를 전달 받는 경우에는 variable parameter list 형태의 interface 를 작성해야 한다.

* Cobol로 부터 전달은 문자열을 이어붙이는 asm 프로그램을 작성한다.

1. TESTASM5.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM5  CSECT
                LR 12,15
                USING TESTASM5,12
                MVC CONTENT(50),=X'00'
                LA  3,0(1)
                LA  5,CONTENT
        *
        LOOP      EQU *
                L   2,0(3)
                MVC WORD,0(2)
                MVC 0(8,5),WORD 
                LA  5,8(5)
                CLI 4(3),X'80'
                BE  FINAL
                LA  3,4(3)
                BNE LOOP
        *
        FINAL     EQU *
                LA  3,4(3)
                L   2,0(3)
                MVC WORD,0(2)
                MVC 0(8,5),WORD
                OFADBGMEM CONTENT,1
                BR  14
        WORD      DS  XL8
        CONTENT   DS  XL50
                END


2. TESTCOB2.cob 이름으로 아래와 같이 cobol 코드를 작성한다.

              ID DIVISION.
              PROGRAM-ID. TESTCOB2.
              DATA DIVISION.
              WORKING-STORAGE SECTION.

              01 L-CONCAT1 PIC X(8) VALUE 'Hello, T'.
              01 L-CONCAT2 PIC X(8) VALUE 'his is a'.
              01 L-CONCAT3 PIC X(8) VALUE ' program'.
              01 L-CONCAT4 PIC X(8) VALUE ' to conc'.
              01 L-CONCAT5 PIC X(8) VALUE 'atenate '.
              01 L-CONCAT6 PIC X(8) VALUE 'string. '.

              01 R-CONCAT1 PIC X(8) VALUE 'Use vari'.
              01 R-CONCAT2 PIC X(8) VALUE 'able par'.
              01 R-CONCAT3 PIC X(8) VALUE 'ameter l'.
              01 R-CONCAT4 PIC X(8) VALUE 'ist.    '.

              PROCEDURE DIVISION.

                  CALL 'TESTASM5' USING L-CONCAT1
                                        L-CONCAT2
                                        L-CONCAT3
                                        L-CONCAT4
                                        L-CONCAT5
                                        L-CONCAT6.

                  CALL 'TESTASM5' USING R-CONCAT1
                                        R-CONCAT2
                                        R-CONCAT3
                                        R-CONCAT4.


3. Entry interface 를 아래와 같이 TESTASM5.json 이름으로 작성한다.

        {
        "entry_list" : [
        {
            "entry_name" : "TESTASM5",
            "variable_parameter_list" : {
            "max_length" : 10
            }
        }
        ],
        "program_name" : "TESTASM5",
        "version" : 3
        }


4. 컴파일 및 디플로이

        $ ofasm TESTASM5.asm
        [OFASMPP: SUCC(0)] TESTASM5.asm
        ofasma: Program size of TESTASM5.asmo = 139 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM5.json
        1 = -i
        2 = TESTASM5.json
        TESTASM5_OFASM_VM_ENTRY.cpp Interface generation complete

        $ g++ -shared -fPIC -o TESTASM5.so TESTASM5_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ ofcob -x TESTCOB2.cob --enable-ofasm


    > Variable parameter list 를 사용하기 위해 ASM를 호출하는 컴파일러 혹은 전처리기에서 특수한 처리가 필요하다. 아래 이를 처리하고 있는 제품 리스트이다.
    > * OFCOBOL
    > * OFPLI
    > * Protrieve
    > * ofcbppf (Netcobol preprocessor)

    > 자세한 내용은 관련 제품의 user guide 를 확인한다.

5. 실행 및 결과 확인

        $ ./TESTCOB2
        Length=[0000000000000050], Hex=[0x48656c6c6f2c205468697320697320612070726f6772616d20746f20636f6e636174656e61746520737472696e672e200000], Char=[Hello, This is a program to concatenate string. ]
        Length=[0000000000000050], Hex=[0x557365207661726961626c6520706172616d65746572206c6973742e20202020000000000000000000000000000000000000], Char=[Use variable parameter list.    ]


### Pointer parameter 로 전달

* 전달 받는 파라미터에 pointer 변수가 존재하는 경우 fixed parameter list 의 pointer type 으로 interface 를 작성해야 한다.

* Cobol로 부터 전달은 문자열에서 대소문자를 치환하고 전달받은 pointer 변수가 가르키는 메모리에 반환하는 프로그램을 작성한다.

1. TESTASM6.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM6 CSECT
                LR 12,15
                USING TESTASM6,12
                ST    14,SAVE
                L     2,0(1)
                USING PARAM,2
                L     5,PTR
                LA    3,CONVERT
        LOOP     EQU   *
                MVC   CHAR,0(2)
                CLI   CHAR,X'00'
                BE    ENDPGM
                BAL   11,CASECHK
                LA    2,1(2)
                BNE   LOOP
        *
        NOTALPHA EQU   *
                OFADBGMEM =C'NOT A ALPHABETIC',1
                B     ENDPGM
        *
        CASECHK  EQU   *
                CLI   CHAR,C'A'
                BL    LOWERCHK
                CLI   CHAR,C'Z'
                BH    LOWERCHK
                B     UPTOLO
        LOWERCHK CLI   CHAR,C'a'
                BL    NOTALPHA
                CLI   CHAR,C'z'
                BH    NOTALPHA
                B     LOTOUP
        * 
        UPTOLO   EQU   *
                AI    CHAR,X'20'
                MVC   0(1,3),CHAR
                LA    3,1(3)
                BR    11
        *
        LOTOUP   EQU   *
                AI    CHAR,X'E0'
                MVC   0(1,3),CHAR
                LA    3,1(3)
                BR    11
        *
        ENDPGM   EQU   *
                MVC   0(26,5),CONVERT
                L     14,SAVE
                BR    14
        SAVE     DS    F
        CHAR     DS    CL1
        CONVERT  DS    CL26
        PARAM    DSECT
                DS    0CL30
        MSG      DS    CL26
        PTR      DS    A(0)
                END


2. TESTCOB3.cob 이름으로 아래와 같이 cobol 코드를 작성한다.

              ID DIVISION.
              PROGRAM-ID. TESTCOB2.
              DATA DIVISION.
              WORKING-STORAGE SECTION.

              01 PARAM.
                  03 MSG PIC X(26) VALUE 'AbCdEfGhIjKlMnOpQrStUVwXyZ'.
                  03 PTR POINTER.

              01 CONVERT PIC X(26).

              PROCEDURE DIVISION.
                  SET PTR TO ADDRESS OF CONVERT.
                  DISPLAY "Before converting : [" MSG "]".
                  CALL "TESTASM6" USING MSG.
                  DISPLAY "After converting : [" CONVERT "]".


3. Entry interface 를 아래와 같이 TESTASM6.json 이름으로 작성한다.

        {
        "entry_list" : [
        {
            "entry_name" : "TESTASM6",
            "fixed_parameter_list" : [
            {
                "param_type" : "P",
                "param_size" : 30,
                "pointer_offset_list" : [ 26 ],
                "pointer_size_list" : [ 26 ]
            }
            ]
        }
        ],
        "program_name" : "TESTASM6",
        "version" : 3
        }


4. 컴파일 및 디플로이

        $ ofasm TESTASM6.asm
        [OFASMPP: SUCC(0)] TESTASM6.asm
        ofasma: Program size of TESTASM6.asmo = 342 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM5.json
        1 = -i
        2 = TESTASM6.json
        TESTASM6_OFASM_VM_ENTRY.cpp Interface generation complete

        $ g++ -shared -fPIC -o TESTASM6.so TESTASM6_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ ofcob -x TESTCOB3.cob


5. 실행 및 결과 확인

        Before converting : [AbCdEfGhIjKlMnOpQrStUVwXyZ]
        After converting : [aBcDeFgHiJkLmNoPqRsTuvWxYz]

----------------------------------------------------------------

### ASM 에서 호출되는 COBOL

#### Fixed parameter 로 전달

* ASM 에서 사칙연산을 수행할 연산자와 피연산자를 Cobol 로 전달하여 사칙연산을 수행하는 프로그램을 작성한다.

* ASM 에서 Cobol 로 파라미터 전달 시 고정 길이, 정해진 수의 파라미터 일 경우 Exit Fixed parameter list 형식의 interface 를 작성한다.

1. TESTASM7.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM7  CSECT
                LR 12,15
                USING TESTASM7,12
                ST    14,SAVE
                MVC   OPERAND1,=F'10'
                MVC   OPERAND2,=F'10'
                MVC   OPERATOR,=C'+'
                CALL  SUBCOB1,(OPERAND1,OPERATOR,OPERAND2)
        *
                MVC   OPERAND1,=F'20'
                MVC   OPERAND2,=F'10'
                MVC   OPERATOR,=C'-'
                CALL  SUBCOB1,(OPERAND1,OPERATOR,OPERAND2)
        *
                MVC   OPERAND1,=F'30'
                MVC   OPERAND2,=F'10'
                MVC   OPERATOR,=C'*'
                CALL  SUBCOB1,(OPERAND1,OPERATOR,OPERAND2)
        *
                MVC   OPERAND1,=F'100'
                MVC   OPERAND2,=F'10'
                MVC   OPERATOR,=C'/'
                CALL  SUBCOB1,(OPERAND1,OPERATOR,OPERAND2)
        *
                L     14,SAVE
                BR 14
        SAVE      DS F
        SUBCOB1   DC C'SUBCOB1'
        OPERAND1  DS F
        OPERATOR  DS CL1
        OPERAND2  DS F
                END


2. SUBCOB1.cob 이름으로 아래와 같이 cobol 코드를 작성한다.

              ID DIVISION.
              PROGRAM-ID. SUBCOB1.
              DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 RESULT PIC S9(8).
              LINKAGE SECTION.
              01 OPERAND1 PIC S9(8) COMP.
              01 OPERATOR PIC X.
              01 OPERAND2 PIC S9(8) COMP.
              PROCEDURE DIVISION USING OPERAND1 OPERATOR OPERAND2.

                  EVALUATE OPERATOR
                      WHEN '+'
                        ADD OPERAND1 TO OPERAND2 GIVING RESULT
                      WHEN '-'
                        SUBTRACT OPERAND2 FROM OPERAND1 GIVING RESULT
                      WHEN '*'
                        MULTIPLY OPERAND1 BY OPERAND2 GIVING RESULT
                      WHEN '/'
                        DIVIDE OPERAND1 BY OPERAND2 GIVING RESULT
                      WHEN OTHER
                        DISPLAY "INVALID OPERATOR"
                  END-EVALUATE.

                  DISPLAY "Answer is : [" RESULT "]".


3. asm 을 호출할 main.cpp 프로그램을 아래와 같이 작성한다.

        extern "C"
        {
        void TESTASM7();
        int main() {
            TESTASM7();
            return 0;
        }
        }


4. main.cpp 에서 asm 호출 시 필요한 entry interface 를 아래와 같이 TESTASM7.json 이름으로 작성한다.

        {
        "entry_list" : [
            {
                "entry_name" : "TESTASM7",
                "fixed_parameter_list" : []
            }
        ],
        "program_name" : "TESTASM7",
        "version" : 3
        }


5. asm 에서 cobol 호출 시 exit interface 를 아래와 같이 SUBCOB1.json 이름으로 작성한다.

        {
        "entry_list" : [
            {
            "entry_name" : "SUBCOB1",
            "fixed_parameter_cnt" : 3
            }
        ],
        "program_name" : "SUBCOB1",
        "version" : 3,
        "interface_type" : "exit"
        }


6. 컴파일

        $ ofasm TESTASM7.asm
        [OFASMPP: SUCC(0)] TESTASM7.asm
        ofasma: Program size of TESTASM7.asmo = 247 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM7.json
        1 = -i
        2 = TESTASM7.json
        TESTASM7_OFASM_VM_ENTRY.cpp Interface generation complete

        $ ofasmif -i SUBCOB1.json
        1 = -i
        2 = SUBCOB1.json
        SUBCOB1_OFASM_VM_EXIT.cpp Interface generation complete

        $ ofcob SUBCOB1.cob -o SUBCOB1.so
        $ g++ -shared -fPIC -o TESTASM7.so TESTASM7_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ g++ -shared -fPIC -o SUBCOB1_OFASM_VM_EXIT.so SUBCOB1_OFASM_VM_EXIT.cpp ./SUBCOB1.so
        $ g++ main.cpp -o main ./TESTASM7.so


7. 실행 및 결과

        $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
        $ ./main
        Answer is : [00000020+]
        Answer is : [00000010+]
        Answer is : [00000300+]
        Answer is : [00000010+]


### Pointer parameter 로 전달

* ASM 에서 전달한 문자열을 Cobol 에서 대소문자를 치환하고 전달받은 pointer 변수가 가르키는 메모리에 반환하는 프로그램을 작성한다.

* ASM 에서 Cobol 전달하는 파라미터에 pointer 변수가 포함되는 경우 Exit fixed parameter list 형식의 pointer param type interface 를 작성한다.


1. TESTASM8.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM8 CSECT
                LR 12,15
                USING TESTASM8,12
                ST 14,SAVE
        *
                LA 2,CONVERT
                ST 2,PTR
                CALL SUBCOB2,(PARAM)
                OFADBGMEM CONVERT,1
        *
                L  14,SAVE
                BR 14
        SAVE     DS F
        SUBCOB2  DC CL7'SUBCOB2'
        CONVERT  DC CL26' '
        PARAM    DS 0CL30
        STR      DC CL26'AbCdEfGhIjKlMnOpQrStUVwXyZ'
        PTR      DS A(0)
                END


2. SUBCOB2.cob 이름으로 아래와 같이 cobol 코드를 작성한다.

              ID DIVISION.
              PROGRAM-ID. SUBCOB2.
              DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 UPPER PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
              01 LOWER PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
              01 IDX PIC 9(4) COMP.
              01 CHAR PIC X.
              LINKAGE SECTION.
              01 CONVERT PIC X(26).
              01 PARAM.
                  03 STR PIC X(26).
                  03 PTR POINTER.
              PROCEDURE DIVISION USING PARAM.
                  SET ADDRESS OF CONVERT TO PTR.
                  PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LENGTH OF STR
                    MOVE STR(IDX:1) TO CHAR
                    IF CHAR >= 'A' AND CHAR <= 'Z'
                      MOVE LOWER(IDX:1) TO CONVERT(IDX:1)
                    ELSE IF CHAR >= 'a' AND CHAR <= 'z'
                      MOVE UPPER(IDX:1) TO CONVERT(IDX:1)
                    ELSE 
                      DISPLAY "NOT A ALPHABETIC"
                    STOP RUN
                  END-PERFORM.
                  GOBACK.


3. asm 를 호출할 main.cpp 프로그램을 아래와 같이 작성한다. 

        extern "C"
        {
        void TESTASM8();
        int main() {
            TESTASM8();
            return 0; 
        }
        }


4. main.cpp 에서 asm 호출 시 필요한 entry interface 를 아래와 같이 TESTASM8.json 이름으로 작성한다.

        {
        "entry_list" : [
            {
                "entry_name" : "TESTASM8",
                "fixed_parameter_list" : []
            }
        ],
        "program_name" : "TESTASM8",
        "version" : 3
        }


5. asm 에서 cobol 호출 시 exit interface 를 아래와 같이 SUBCOB2.json 이름으로 작성한다. 

        {
        "entry_list" : [
            {
            "entry_name" : "SUBCOB2",
            "fixed_parameter_list" : [
                    {
                        "param_type" : "P",
                        "param_size" : 34,
                        "pointer_offset_list" : [ 26 ],
                        "pointer_size_list" : [ 26 ]
                    }
                ]
            }
        ],
        "program_name" : "SUBCOB2",
        "version" : 3,
        "interface_type" : "exit"
        }


6. 컴파일

        $ ofasm TESTASM8.asm
        [OFASMPP: SUCC(0)] TESTASM8.asm
        ofasma: Program size of TESTASM8.asmo = 117 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM8.json
        1 = -i
        2 = TESTASM8.json
        TESTASM8_OFASM_VM_ENTRY.cpp Interface generation complete

        $ ofasmif -i SUBCOB2.json
        1 = -i
        2 = SUBCOB2.json
        SUBCOB2_OFASM_VM_EXIT.cpp Interface generation complete

        $ ofcob SUBCOB2.cob -o SUBCOB2.so
        $ g++ -shared -fPIC -o TESTASM8.so TESTASM8_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ g++ -shared -fPIC -o SUBCOB2_OFASM_VM_EXIT.so SUBCOB2_OFASM_VM_EXIT.cpp ./SUBCOB2.so
        $ g++ main.cpp -o main ./TESTASM8.so


7. 실행 및 결과

        $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
        $ ./main

        Length=[0000000000000026], Hex=[0x6142634465466748694a6b4c6d4e6f507152735475765778597a], Char=[aBcDeFgHiJkLmNoPqRsTuvWxYz]


    > EXIT interface 에 호출된 application 으로 ofcobol 뿐만 아니라 OFPLI, Netcobol, C/C++ 으로 작성된 application 도 호출 가능하다.

----------------------------------------------------------------

## Dataset 운용

### NonVsam dataset I/O

* 사원 리스트 DATASET 를 Read 하여 사번이 2016 이하인 사원만 OUTPUT DATASET에 Write 하는 asm 프로그램을 작성한다.

1. TESTASM9.asm 이름으로 아래와 같이 asm 코드를 작성한다.

        TESTASM9 CSECT
                LR 12,15
                USING TESTASM9,12
                ST 14,SAVE
        *
                OPEN  (INDCB,(INPUT))
                OPEN  (OTDCB,(OUTPUT))
        *
        LOOP     EQU   *
                GET   INDCB,IOAREA
                L     2,YYYY
                C     2,=CL4'2016'
                BLE   PUTEMP
                BH    LOOP
        PUTEMP   EQU   *
                PUT   OTDCB,IOAREA
                B     LOOP
        *
        EXIT     EQU   *
                CLOSE INDCB
                CLOSE OTDCB
        *
                L 14,SAVE
                BR    14
        *
        INDCB    DCB DDNAME=INDD,LRECL=80,BLKSIZE=1,DSORG=PS,MACRF=GM,         X
                    EODAD=EXIT
        OTDCB    DCB DDNAME=OUTDD,LRECL=80,BLKSIZE=1,DSORG=PS,MACRF=PM
        IOAREA   DS    0CL80
        EMPNO    DS    0CL8
        YYYY     DS    CL4
        SEQNO    DS    CL4
        TEAM     DS    CL11
        NAME     DS    CL14
        TITLE    DS    CL11
        POS      DS    CL12
        EMAIL    DS    CL24
        SAVE     DS    F
                LTORG
                END


2. TEST.INPUT.DATA 를 아래와 같이 작성한다.

        2013117 MW3-5 Team Lee So-young  Researcher Team Leader soyoung_lee@tmax.co.kr  
        2016240 MW3-5 Team Junho Jung    QS Manager Team member joonho_jung@tmax.co.kr  
        2017145 MW3-5 Team Lee Min-seong Researcher Team member minseong_lee@tmax.co.kr 
        2013079 MW3-5 Team Lim Hee-chun  Researcher Team member heechun_lim@tmax.co.kr  
        2014040 MW3-5 Team Dongkyun Kim  Researcher Team member donggyun_kim2@tmax.co.kr
        2016049 MW3-5 Team Shim Kyung-ju Researcher Team member kyoungju_sim@tmax.co.kr 
        2018312 MW3-5 Team Cha Hyun-in   Researcher Team member hyunin_cha@tmax.co.kr   
        2018309 MW3-5 Team Jinseong Oh   Researcher Team member jinseong_oh@tmax.co.kr 

3. 데이터셋을 생성하고 데이터셋을 저장 한다.

        $ dscreate TEST.INPUT.DATA
        dscreate version 7.0.3(8) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Create a New Dataset or a Member of PDS Dataset

        DSCREATE DSNAME=TEST.INPUT.DATA,CATALOG=,VOLSER=,MEMBER=
        OFRUISVRDSCRE: Dataset Create OK. dsn=TEST.INPUT.DATA
        COMPLETED SUCCESSFULLY.

        $ dssave TEST.INPUT.DATA -s $PWD/TEST.INPUT.DATA -d NEWLINE
        dssave version 7.0.3(8) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Dataset Save Program for External Editor

        DSSAVE
        Source File        : [/home/oframe7/test/file/TEST.INPUT.DATA]
        Destination Dataset: [TEST.INPUT.DATA]
        Destination Member : []
        User Catalog       : []
        Volume Serial      : []
        Delimiter          : [\n]

        OFRUISVRDSSAVE: Dataset Is Saved Successfully
        COMPLETED SUCCESSFULLY.


4. TESTJCL2 이름으로 jcl 를 작성한다.

        //TESTJCL2 JOB
        //JOBLIB   DD DSN=SYS1.USERLIB,DISP=SHR
        //*************************************
        //DELE00   EXEC PGM=IEFBR14
        //INIT     DD DSN=TEST.OUTPUT.DATA,DISP=(MOD,DELETE,DELETE)
        //*************************************
        //STEP00   EXEC PGM=TESTASM9
        //INDD     DD DSN=TEST.INPUT.DATA,DISP=SHR
        //OUTDD    DD DSN=TEST.OUTPUT.DATA,DISP=(NEW,CATLG,DELETE)
        //SYSOUT   DD SYSOUT=*
        //


5. 컴파일 및 디플로이

        $ ofasm TESTASM9.asm
        [OFASMPP: SUCC(0)] TESTASM9.asm
        ofasma: Program size of TESTASM9.asmo = 392 byte
        ofasma: assembly success

        $ ofasmif -i TESTASM9.json
        1 = -i
        2 = TESTASM9.json
        TESTASM9_OFASM_VM_ENTRY.cpp Interface generation complete

        $ g++ -shared -fPIC -o TESTASM9.so TESTASM9_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ dlupdate /home/oframe7/test/file/TESTASM9.so SYS1.USERLIB
        $ dlupdate /home/oframe7/test/file/TESTASM9.asmo SYS1.USERLIB


6. Jcl 수행 및 수행 결과 확인

        $ tjesmgr r $PWD/TESTJCL2
        >
        Command : [r /home/oframe7/test/file/TESTJCL2]
        Node name :  A N Y
        (JOB00019) /home/oframe7/test/file/TESTJCL2 is submitted as TESTJCL2(JOB00019).

        $ tjesmgr psj JOB00019
        >
        Command : [psj JOB00019]
        JOB  ID    : JOB00019      NODE NAME  : NODE1   
        JOB  NAME  : TESTJCL2
        JOB  CLASS : A , JOB STATUS : Done(R00000) , JOB PRTY : 5 , JCLRUNNER INDEX : 5
        JOB  USER  : ROOT     
        JCL  PATH  : /home/oframe7/test/file/TESTJCL2
        TIME STAMP : READY : 20210715/06:44:48, START : 20210715/06:44:49
                                                END   : 20210715/06:44:50
        RES  USAGE : PROCESS - 1s CPU - (0s,0%)  MEM - 0Byte
        ----------------------------------------------------------------------------
        [  DELE00] START : 20210715/06:44:49, RC=R0000, CPU - 0s ( / 0s)
        [  STEP00] START : 20210715/06:44:49, RC=R0000, CPU - 0s ( / 0s)
        ----------------------------------------------------------------------------
        
        SPOOL LIST : 
        -----------------------------------------------------------------------------
        NO  STEP              DDNAME       SIZE  DSNAME
        -----------------------------------------------------------------------------
        0   --------          INPJCL         380 INPJCL                          
        1   --------          SYSMSG          3K SYSMSG                          
        3   --------          CONVJCL        377 CONVJCL                         
        4   --------          JESMSG         890 JESMSG                          
        5   --------          JESJCL          1K JESJCL                          
        -----------------------------------------------------------------------------
        
        OUTPUT PROCESSING STATUS : all outputs were processed


7. OUTPUT 데이터셋 확인

        $ dsview TEST.OUTPUT.DATA
        00000001: 2013117 MW3-5 Team Lee So-young  Researcher Team Leader soyoung_lee@tmax.co.kr
        00000002: 2016240 MW3-5 Team Junho Jung    QS Manager Team member joonho_jung@tmax.co.kr
        00000003: 2013079 MW3-5 Team Lim Hee-chun  Researcher Team member heechun_lim@tmax.co.kr
        00000004: 2014040 MW3-5 Team Dongkyun Kim  Researcher Team member donggyun_kim2@tmax.co.kr
        00000005: 2016049 MW3-5 Team Shim Kyung-ju Researcher Team member kyoungju_sim@tmax.co.kr
        ----END----


### Vsam dataset I/O

* Vsam 중 KEY 를 이용하는 KSDS 데이터셋을 예를들어 설명한다.

* 사원 리스트 Dataset 를 정해진 KeyTable 에 속한 사원만을 Read 하여 OUTPUT Dataset 에 Write 하는 asm 프로그램을 작성한다.

1. VSAMASM.asm 이름으로 아래와 같이 asm 코드를 작성한다

        VSAMASM CSECT
                LR 12,15
                USING VSAMASM,12
                ST 14,SAVE
        *
                OPEN KSACB
                OPEN (OTDCB,(OUTPUT))
                LA  3,KEYTABLE
        *
        SETKEY   EQU *
                LG  4,0(3)
                CG  4,=XL8'FFFFFFFFFFFFFFFF'
                BE  EXIT
                MVC KEYAREA(8),0(3) 
                LA  3,8(3)
        GETREC   EQU *
                GET RPL=KSRPL
                LTR 15,15
                BNZ ERROR
                PUT OTDCB,IOAREA
                MVC KEYAREA,EMPNO
                B   SETKEY
        *
        ERROR    EQU *
                OFADBGMEM =C'VSAM FILE I/O FAIL',1
                L  15,=F'255'                                                         
                B EXIT
        *
        EXIT     EQU *
                CLOSE KSACB
                CLOSE OTDCB
                L 14,SAVE
                BR 14
        *
        KEYTABLE DS 0CL24
                DC CL8'2017145'
                DC CL8'2016240'
                DC XL8'FFFFFFFFFFFFFFFF'
        KSACB    ACB DDNAME=KSACB,MACRF=(KEY,DIR,IN)
        KSRPL    RPL ACB=KSACB,AREA=IOAREA,AREALEN=80,ARG=KEYAREA,             X
                    KEYLEN=(8),OPTCD=(KEY,DIR,MVE,KEQ)
        OTDCB    DCB DDNAME=OUTDD,LRECL=80,DSORG=PS,MACRF=PM
        SAVE     DS F
        KEYAREA  DS CL8
        IOAREA   DS 0CL80
        EMPNO    DS CL8
        TEAM     DS CL11
        NAME     DS CL14
        TITLE    DS CL11
        POS      DS CL12
        EMAIL    DS CL24
                END


2. Vsam KSDS 데이터셋인 TEST.INPUT.DATA 의 copybook 을 아래 내용으로 $OPENFRAME_HOME/tsam/copybook/TEST.INPUT.DATA.cpy 에 저장한다.

              01 EMP-INFO.
                  03 EMPNO     PIC X(8). 
                  03 TEAM      PIC X(11). 
                  03 NAME      PIC X(14).
                  03 TITLE     PIC X(11).
                  03 POS       PIC X(12).
                  03 EMAIL     PIC X(24).
 

3. TEST.INPUT.DATA.SAM 를 아래와 같이 작성한다.

        2013117 MW3-5 Team Lee So-young  Researcher Team Leader soyoung_lee@tmax.co.kr  
        2016240 MW3-5 Team Junho Jung    QS Manager Team member joonho_jung@tmax.co.kr  
        2017145 MW3-5 Team Lee Min-seong Researcher Team member minseong_lee@tmax.co.kr 
        2013079 MW3-5 Team Lim Hee-chun  Researcher Team member heechun_lim@tmax.co.kr  
        2014040 MW3-5 Team Dongkyun Kim  Researcher Team member donggyun_kim2@tmax.co.kr
        2016049 MW3-5 Team Shim Kyung-ju Researcher Team member kyoungju_sim@tmax.co.kr 
        2018312 MW3-5 Team Cha Hyun-in   Researcher Team member hyunin_cha@tmax.co.kr   
        2018309 MW3-5 Team Jinseong Oh   Researcher Team member jinseong_oh@tmax.co.kr 

4. TEST.INPUT.DATA KSDS 데이터셋을 생성하고 데이터셋을 저장 한다.


        $ dscreate TEST.INPUT.DATA.SAM
        dscreate version 7.0.3(8) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Create a New Dataset or a Member of PDS Dataset

        DSCREATE DSNAME=TEST.INPUT.DATA.SAM,CATALOG=,VOLSER=,MEMBER=
        OFRUISVRDSCRE: Dataset Create OK. dsn=TEST.INPUT.DATA.SAM
        COMPLETED SUCCESSFULLY.

        $ dssave TEST.INPUT.DATA.SAM -s $PWD/TEST.INPUT.DATA.SAM -d NEWLINE
        dssave version 7.0.3(8) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Dataset Save Program for External Editor

        DSSAVE
        Source File        : [/home/oframe7/test/file/TEST.INPUT.DATA.SAM]
        Destination Dataset: [TEST.INPUT.DATA.SAM]
        Destination Member : []
        User Catalog       : []
        Volume Serial      : []
        Delimiter          : [\n]

        OFRUISVRDSSAVE: Dataset Is Saved Successfully
        COMPLETED SUCCESSFULLY.

        $ idcams define CL -n TEST.INPUT.DATA -o KS -k 8,0 -l 80,80
        idcams version 7.0.3(13) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Access Method Services for Catalogs

        IDCAMS COMMAND=DEFINE,TYPE=CL,NAME=TEST.INPUT.DATA,RELATE=,CATALOG=

        tbESQL Precompiler 6  

        TmaxData Corporation Copyright (c) 2008-. All rights reserved.


        TEST_INPUT_DATA.tbc is precompiled successfully!

        COMPLETED SUCCESSFULLY.

        $ idcams repro -i TEST.INPUT.DATA.SAM -o TEST.INPUT.DATA 
        idcams version 7.0.3(13) obuild@tplinux64:ofsrc7/base(#1) 2020-12-29 11:52:19
        Access Method Services for Catalogs

        IDCAMS COMMAND=REPRO,INDATASET=TEST.INPUT.DATA.SAM,OUTDATASET=TEST.INPUT.DATA,CATALOG=
        REPRO 8 record(s)
        COMPLETED SUCCESSFULLY.


5. TESTJCL3 이름으로 jcl 를 작성한다.

        //TESTJCL3 JOB
        //JOBLIB   DD DSN=SYS1.USERLIB,DISP=SHR
        //*************************************
        //DELE00   EXEC PGM=IEFBR14
        //INIT     DD DSN=TEST.OUTPUT.DATA,DISP=(MOD,DELETE,DELETE)
        //*************************************
        //STEP00   EXEC PGM=VSAMASM
        //KSACB    DD DSN=TEST.INPUT.DATA,DISP=SHR
        //OUTDD    DD DSN=TEST.OUTPUT.DATA,DISP=(NEW,CATLG,DELETE)
        //SYSOUT   DD SYSOUT=*
        //


6. 컴파일 및 디플로이

        $ ofasm VSAMASM.asm
        [OFASMPP: SUCC(0)] VSAMASM.asm
        ofasma: Program size of VSAMASM.asmo = 546 byte
        ofasma: assembly success

        $ ofasmif -i VSAMASM.json
        1 = -i
        2 = VSAMASM.json
        VSAMASM_OFASM_VM_ENTRY.cpp Interface generation complete

        $ g++ -shared -fPIC -o VSAMASM.so VSAMASM_OFASM_VM_ENTRY.cpp -L/home/oframe7/data/install/lib -lofasmVM
        $ dlupdate /home/oframe7/test/file/VSAMASM.so SYS1.USERLIB
        $ dlupdate /home/oframe7/test/file/VSAMASM.asmo SYS1.USERLIB


7. Jcl 수행 및 수행 결과 확인

        $ tjesmgr r $PWD/TESTJCL3
        >
        Command : [r /home/oframe7/test/file/TESTJCL3]
        Node name :  A N Y
        (JOB00028) /home/oframe7/test/file/TESTJCL3 is submitted as TESTJCL3(JOB00028).


8. OUTPUT 데이터셋 확인

        $ dsview TEST.OUTPUT.DATA

        00000001: 2017145 MW3-5 Team Lee Min-seong Researcher Team member minseong_lee@tmax.co.kr
        00000002: 2016240 MW3-5 Team Junho Jung    QS Manager Team member joonho_jung@tmax.co.kr
        ----END----


    > dscreate : 새 데이터셋 또는 PDS 데이터셋의 구성 데이터셋을 만들기 위해 사용된다. \
    > dssave : Unix 파일을 데이터셋에 저장하기 위해 사용된다. \
    > idcams : Vsam 데이터셋을 조작하기 위해 사용된다.