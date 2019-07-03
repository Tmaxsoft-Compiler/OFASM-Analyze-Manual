# OFASM 자산 분석 매뉴얼

## 목차

[1. 프로젝트 진행 전 고려해야 할 사항](#프로젝트-진행-전-고려해야-할-사항)

[2. 프로젝트 진행 중 발생하는 문제들](#프로젝트-진행-중-발생하는-문제들)

* * *

## 프로젝트 진행 전 고려해야 할 사항

[1. 32비트/64비트 바이너리](#32비트/64비트-바이너리)
 
[2. OFASM이 지원하는 명령어](#OFASM이-지원하는-명령어)
 
[3. 어셈블러 소스를 수정해야 하는 상황](#어셈블러-소스를-수정해야-하는-상황)


### 32비트/64비트 바이너리

OFASM은 기본적으로 32비트 바이너리 사용을 권장한다. 

아래와 같은 상황에서는 64비트 바이너리를 사용해도 무방하다.
<pre>
1. 어셈블러 자산의 프로젝트 SCOPE가 명확하며, 더이상 자산이 추가될 가능성이 없다
2. 어셈블러 프로그램이 외부(코볼 혹은 PL/I 등 다른 언어로 작성된) 프로그램으로 포인터 변수(주소값)를 전달 하지 않는다.
3. 어셈블러 프로그램이 외부 프로그램으로부터 포인터 변수를 전달 받지 않는다.
4. OSC를 함께 사용하는 경우, COMMAREA 등의 CICS 시스템 영역에 포인터 변수를 포함하여 데이터를 송수신 하지 않는다.
</pre>

## 프로젝트 진행 중 어셈블러 자산 분석 과정

프로젝트가 시작하면 어셈블러 자산은 아래와 같은 과정을 거쳐서 진행된다.

[1. 전체 자산 컴파일 및 에러 메세지 확인](#전체-자산-컴파일-및-에러-메세지-확인)

[2. 시스템 매크로 및 SVC 사용 목록 확인](#시스템-매크로-및-SVC-사용-목록-확인)

[3. CICS 커맨드 확인](#CICS-커맨드-확인)

[4. 포인터 파라미터 사용 여부 확인](#포인터-파라미터-사용-여부-확인)

[5. EBCDIC 문자 확인](#EBCDIC-문자-확인)

[6. 인터페이스 작성 방법](#인터페이스-작성-방법)

### 전체 자산 컴파일 및 에러 메세지 확인

### 시스템 매크로 및 SVC 사용 목록 확인

### CICS 커맨드 확인

우선, CICS에서 사용되는 자산인지 판단하기 위해서 자산에서 CICS 명령어를 사용하는지 확인한다. CICS 명령어는 아래와 같은 형태를 가지고 사용된다. 

<pre>
    EXEC CICS <명령어> <옵션1> <옵션2> ...
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

> 1. OSC 담당자에게 cicsprep을 통한 전처리 기능을 요청한다.
> 2. OSC 담당자에게 해당 CICS 명령어에 대한 기능을 요청한다.

### 포인터 파라미터 사용 여부 확인

### EBCDIC 문자 확인

### 인터페이스 작성 방법

OFASM을 이용하여 어셈블러 프로그램을 실행하기 위해서는 OFASM VM을 실행시키기 위한 인터페이스 프로그램이 필요하다.

인터페이스에 관한 내용은 아래를 참고한다.

//TODO: 인터페이스 관련 문서 링크 추가

## OFASM이 지원하는 명령어

OFASM이 지원하는 명령어는 아래 리스트를 참고한다.

* Preprocessing (Conditional assembly) Instruction

| Instruction Name | OFASM testcase | note |
| --- | --- | --- |
| ACTR | O | | 
| AGO | O | | 
| AIF | O | | 
| ANOP | O | |
| GBLA | X | |
| GBLB | X | |
| GBLC | X | |
| LCLA | O | |
| LCLB | O | |
| LCLC | O | |
| SETA | O | |
| SETB | O | |
| SETC | O | data attribute O'(Operation Code) 미지원 |

* Assembler Instruction

| Instruction Name | OFASM testcase | note |
| --- | --- | --- |
| AMODE | | |
| CCW | | |
| CCW0 | | |
| COPY | O | |
| CNOP | | |
| CSECT | O | |
| DC | O | |
| DROP | | |
| DS | O | |
| DSECT | O | |
| EJECT | | |
| END | O | |
| ENTRY | O | |
| EQU | O | |
| EXTRN | | |
| ISEQ | | |
| LOCTR | O | |
| LTORG | | |
| MNOTE | O | |
| OPSYN | O | |
| ORG | O | |
| POP | O | |
| PRINT | | |
| PUNCH | | |
| PUSH | | |
| RMODE | | |
| SPACE | | |
| START | O | |
| TITLE | | |
| USING | O | |
| WXTRN | | |

* Machine Instruction

| Instruction Name | OFASM testcase | note |
| --- | --- | --- |
|	A	|	O	|	|
|	AG	|	O	|	|
|	AGHI	|	O	|	|
|	AGR	|	O	|	|
|	AH	|	O	|	|
|	AHI	|	O	|	|
|	AL	|	O	|	|
|	ALGR	|	O	|	|
|	ALR	|	O	|	|
|	ALY	|	O	|	|
|	AP	|	O	|	|
|	AR	|	O	|	|
|	BAKR	|	O	|	|
|	BAL	|		|	|
|	BALR	|		|	|
|	BAS	|	O	|	|
|	BASR	|	O	|	|
|	BASSM	|		|	|
|	BC	|	O	|	|
|	BCR	|	O	|	|
|	BCT	|	O	|	|
|	BCTR	|		|	|
|	BRAS	|	O	|	|
|	BRC	|	O	|	|
|	BRCL	|		|	|
|	BRCT	|	O	|	|
|	BRXH	|	O	|	|
|	BRXLE	|	O	|	|
|	BSM	|	O	|	|
|	BXH	|	O	|	|
|	BXLE	|	O	|	|
|	C	|	O	|	|
|	CDS	|	O	|	|
|	CG	|	O	|	|
|	CGHI	|	O	|	|
|	CGR	|	O	|	|
|	CH	|	O	|	|
|	CHI	|	O	|	|
|	CKSM	|	O	|	|
|	CL	|	O	|	|
|	CLC	|	O	|	|
|	CLCL	|	O	|	|
|	CLFI	|	O	|	|
|	CLG	|	O	|	|
|	CLGFR	|	O	|	|
|	CLGR	|	O	|	|
|	CLHHSI	|	O	|	|
|	CLI	|	O	|	|
|	CLIY	|	O	|	|
|	CLM	|	O	|	|
|	CLMY	|	O	|	|
|	CLR	|	O	|	|
|	CLST	|	O	|	|
|	CLY	|	O	|	|
|	CP	|	O	|	|
|	CPYA	|		|	|
|	CR	|	O	|	|
|	CS	|	O	|	|
|	CSG	|	O	|	|
|	CVB	|	O	|	|
|	CVBG	|	O	|	|
|	CVBY	|	O	|	|
|	CVD	|	O	|	|
|	CVDG	|	O	|	|
|	D	|	O	|	|
|	DL	|	O	|	|
|	DLG	|	O	|	|
|	DLGR	|	O	|	|
|	DLR	|	O	|	|
|	DP	|	O	|	|
|	DR	|	O	|	|
|	DSGF	|	O	|	|
|	ED	|		|	|
|	EDMK	|		|	|
|	EREG	|		|	|
|	EX	|	O	|	|
|	IC	|		|	|
|	ICM	|	O	|	|
|	ICMH	|	O	|	|
|	ICMY	|	O	|	|
|	IIHF	|	O	|	|
|	IIHH	|	O	|	|
|	IIHL	|	O	|	|
|	IILF	|	O	|	|
|	IILH	|	O	|	|
|	IILL	|	O	|	|
|	IPK	|	O	|	|
|	IPM	|	O	|	|
|	JAS	|	O	|	|
|	JC	|		|	|
|	JCT	|	O	|	|
|	JE	|		|	|
|	JXH	|		|	|
|	JXLE	|		|	|
|	KLMD	|	O	|	|
|	L	|	O	|	|
|	LA	|		|	|
|	LAE	|		|	|
|	LAM	|		|	|
|	LARL	|	O	|	|
|	LAY	|	O	|	|
|	LCR	|		|	|
|	LD	|	O	|	|
|	LG	|	O	|	|
|	LGB	|	O	|	|
|	LGF	|	O	|	|
|	LGH	|	O	|	|
|	LGHI	|	O	|	|
|	LGR	|	O	|	|
|	LH	|		|	|
|	LHI	|	O	|	|
|	LHY	|	O	|	|
|	LLC	|	O	|	|
|	LLGC	|	O	|	|
|	LLGF	|	O	|	|
|	LLGFR	|	O	|	|
|	LLGH	|	O	|	|
|	LLGT	|	O	|	|
|	LLH	|	O	|	|
|	LLILF	|	O	|	|
|	LLILH	|	O	|	|
|	LLILL	|	O	|	|
|	LM	|	O	|	|
|	LMD	|	O	|	|
|	LMG	|	O	|	|
|	LMH	|	O	|	|
|	LNR	|	O	|	|
|	LPR	|	O	|	|
|	LR	|	O	|	|
|	LRVG	|	O	|	|
|	LT	|	O	|	|
|	LTGR	|	O	|	|
|	LTR	|	O	|	|
|	LY	|	O	|	|
|	M	|	O	|	|
|	MH	|	O	|	|
|	MHI	|	O	|	|
|	ML	|	O	|	|
|	MP	|	O	|	|
|	MR	|	O	|	|
|	MSGF	|	O	|	|
|	MSR	|	O	|	|
|	MVC	|	O	|	|
|	MVCL	|	O	|	|
|	MVI	|	O	|	|
|	MVIY	|	O	|	|
|	MVN	|	O	|	|
|	MVO	|	O	|	|
|	MVST	|	O	|	|
|	MVZ	|		|	|
|	N	|	O	|	|
|	NC	|	O	|	|
|	NG	|	O	|	|
|	NGR	|	O	|	|
|	NI	|	O	|	|
|	NIHH	|	O	|	|
|	NILH	|		|	|
|	NILL	|		|	|
|	NR	|	O	|	|
|	O	|	O	|	|
|	OC	|	O	|	|
|	OI	|	O	|	|
|	OIHH	|		|	|
|	OIHL	|		|	|
|	OILH	|		|	|
|	OILL	|		|	|
|	OIY	|	O	|	|
|	OR	|	O	|	|
|	PACK	|	O	|	|
|	PC	|	O	|	|
|	PKA	|	O	|	|
|	PR	|	O	|	|
|	RLLG	|		|	|
|	S	|	O	|	|
|	SAC	|		|	|
|	SAM24	|	O	|	|
|	SAM31	|		|	|
|	SAM64	|		|	|
|	SG	|	O	|	|
|	SGF	|	O	|	|
|	SH	|	O	|	|
|	SL	|	O	|	|
|	SLA	|	O	|	|
|	SLBGR	|	O	|	|
|	SLDA	|	O	|	|
|	SLDL	|	O	|	|
|	SLG	|	O	|	|
|	SLGR	|	O	|	|
|	SLL	|	O	|	|
|	SLR	|	O	|	|
|	SP	|	O	|	|
|	SPM	|	O	|	|
|	SR	|	O	|	|
|	SRA	|	O	|	|
|	SRAG	|	O	|	|
|	SRDA	|	O	|	|
|	SRDL	|	O	|	|
|	SRL	|	O	|	|
|	SRLG	|		|	|
|	SRP	|	O	|	|
|	SRST	|	O	|	|
|	SSK	|		|	|
|	SSKE	|		|	|
|	SSM	|		|	|
|	ST	|		|	|
|	STC	|	O	|	|
|	STCK	|	O	|	|
|	STCKE	|	O	|	|
|	STCM	|		|	|
|	STCMH	|	O	|	|
|	STD	|	O	|	|
|	STG	|	O	|	|
|	STH	|	O	|	|
|	STHY	|	O	|	|
|	STM	|	O	|	|
|	STMG	|	O	|	|
|	STMH	|	O	|	|
|	STY	|	O	|	|
|	SVC	|		|	|
|	SW	|	O	|	|
|	TAM	|		|	|
|	TM	|	O	|	|
|	TMLH	|	O	|	|
|	TMLL	|	O	|	|
|	TMY	|	O	|	|
|	TP	|	O	|	|
|	TPROT	|	O	|	|
|	TR	|		|	|
|	TRAP2	|		|	|
|	TRT	|		|	|
|	TS	|	O	|	|
|	UNPK	|	O	|	|
|	UNPKA	|	O	|	|
|	X	|	O	|	|
|	XC	|	O	|	|
|	XG	|	O	|	|
|	XGR	|	O	|	|
|	XI	|	O	|	|
|	XR	|	O	|	|
|	ZAP	|	O	|	|


* Extended Mnemonic Instruction

| Instruction Name | OFASM testcase | note |
| --- | --- | --- |
|	B	|		|	|
|	BE	|	O	|	|
|	BER	|		|	|
|	BH	|	O	|	|
|	BHE	|	O	|	|
|	BHER	|		|	|
|	BHR	|	O	|	|
|	BL	|	O	|	|
|	BLE	|	O	|	|
|	BLER	|		|	|
|	BLH	|	O	|	|
|	BLHR	|		|	|
|	BLR	|	O	|	|
|	BM	|	O	|	|
|	BMR	|	O	|	|
|	BNE	|	O	|	|
|	BNER	|	O	|	|
|	BNH	|	O	|	|
|	BNHE	|	O	|	|
|	BNHER	|		|	|
|	BNHR	|		|	|
|	BNL	|	O	|	|
|	BNLE	|	O	|	|
|	BNLER	|		|	|
|	BNLH	|	O	|	|
|	BNLHR	|		|	|
|	BNLR	|		|	|
|	BNM	|	O	|	|
|	BNMR	|		|	|
|	BNO	|		|	|
|	BNOR	|		|	|
|	BNP	|	O	|	|
|	BNPR	|		|	|
|	BNZ	|	O	|	|
|	BNZR	|		|	|
|	BO	|		|	|
|	BOR	|		|	|
|	BP	|	O	|	|
|	BPR	|		|	|
|	BR	|		|	|
|	BRE	|		|	|
|	BREL	|		|	|
|	BRH	|		|	|
|	BRHE	|		|	|
|	BRHL	|		|	|
|	BRL	|		|	|
|	BRLE	|		|	|
|	BRLH	|		|	|
|	BRLL	|		|	|
|	BRM	|		|	|
|	BRNE	|		|	|
|	BRNEL	|		|	|
|	BRNH	|		|	|
|	BRNHE	|		|	|
|	BRNHL	|		|	|
|	BRNL	|		|	|
|	BRNLE	|		|	|
|	BRNLH	|		|	|
|	BRNLL	|		|	|
|	BRNM	|		|	|
|	BRNO	|		|	|
|	BRNOL	|		|	|
|	BRNOP	|		|	|
|	BRNP	|		|	|
|	BRNZ	|		|	|
|	BRO	|		|	|
|	BROL	|		|	|
|	BRP	|		|	|
|	BRU	|		|	|
|	BRUL	|		|	|
|	BRZ	|		|	|
|	BZ	|	O	|	|
|	BZR	|		|	|
|	J	|	O	|	|
|	JH	|		|	|
|	JHE	|		|	|
|	JL	|		|	|
|	JLE	|		|	|
|	JLH	|		|	|
|	JLNOP	|		|	|
|	JM	|		|	|
|	JNE	|		|	|
|	JNE	|		|	|
|	JNH	|		|	|
|	JNHE	|		|	|
|	JNL	|		|	|
|	JNLE	|		|	|
|	JNLH	|		|	|
|	JNM	|		|	|
|	JNO	|		|	|
|	JNOP	|	O	|	|
|	JNP	|		|	|
|	JNZ	|		|	|
|	JO	|		|	|
|	JP	|		|	|
|	JUMP	|		|	|
|	JZ	|		|	|
|	NOP	|	O	|	|
|	NOPR	|		|	|

* SVC Macro
** 아래는 현재 OFASM에서 지원하고 있는 SVC 및 그와 연관된 매크로들이다.

| SVC number | macro name | note |
| --- | --- | --- |
|	SVC 0	|	EXCP/XDAP	|		|
|	SVC 1	|	WAIT/WAITR/PRTOV	|		|
|	SVC 4	|	GETMAIN	|		|
|	SVC 5	|	FREEMAIN	|		|
|	SVC 6	|	LINK/LINKX	|		|
|	SVC 8	|	LOAD	|		|
|	SVC 9	|	DELETE	|		|
|	SVC 10	|	GETMAIN/FREEMAIN with R operand	|		|
|	SVC 11	|	TIME	|	flag 0x02 (HHMMSShh)만 지원	|
|	SVC 13	|	ABEND	|	ABEND Completion code 사용	|
|	SVC 14	|	SPIE	|		|
|	SVC 18	|	BLDL/FIND (type D)	|		|
|	SVC 19	|	OPEN	|		|
|	SVC 20	|	CLOSE	|		|
|	SVC 21	|	STOW	|	ADD,REPLACE,DELETE,CHANGE,INIT 지원	|
|	SVC 22	|	OPEN (TYPE=J)	|		|
|	SVC 24	|	DEVTYPE	|		|
|	SVC 26	|	CATALOG/INDEX/LOCATE	|	CAMLIST: UNCATALOG,NAME 만 지원	|
|	SVC 27	|	OBTAIN	|		|
|	SVC 29	|	SCRATCH	|	CAMLIST: SCRATCH 만 지원	|
|	SVC 35	|	WTO/WTOR	|		|
|	SVC 40	|	EXTRACT	|	FIELDS=(TIOT/TSO) 만 지원	|
|	SVC 47	|	STIMER/STIMERM	|	BINTVL,DINTVL 만 지원	|
|	SVC 48	|	DEQ	|		|
|	SVC 51	|	SNAP or SNAPX/SDUMP or SDUMPX	|		|
|	SVC 56	|	ENQ	|		|
|	SVC 64	|	RDJFCB	|		|
|	SVC 99	|	DYNALLOC	|		|
|	SVC 120	|	GETMAIN/FREEMAIN	|	flag 4만 지원 : 0x10 (24bit),0x30 (31bit)만 지원	|

* CICS 

** 아래는 OFASM 내부적으로 지원 하거나, 내부 동작을 거친 후에 cics 관련 함수를 호출하는 CICS 명령어들이다.

| CICS type | CICS func | note |
| --- | --- | --- |
|	HANDLE CONDITION	|		|		|
|	GETMAIN	|	cics_getmain	|		|
|	FREEMAIN	|	cics_freemain	|		|
|	ADDRESS	|	cics_address	|		|
|	LOAD	|	cics_load	|		|
|	RELEASE	|	cics_release	|		|
|	LINK	|		|		|
|	READ	|	cics_read	|		|
|	READQ TS	|	cics_readq_ts	|		|
|	READQ TD	|	cics_readq_td	|		|
|	READNEXT	|	cics_readnext	|		|
|	READPREV	|	cics_readprev	|		|
|	RECEIVE	|	cics_receive	|		|
|	RECEIVE MAP	|	cics_recive_map	|		|
|	RETRIEVE	|	cics_retrieve	|		|
|	SPOOLREAD	|	cics_spoolread	|		|

** 아래는 OFASM 내부적으로 지원하는 CICS 관련 카피북/매크로이다.

|	CICS copybook	|	note	|
|	---	|	---	|
|	DFHAID	|		|
|	DFHBMSCA	|		|
|	DFHEIBLK	|		|
|	DFHGDEFS	|		|
|	DFHCSADS	|		|

|	CICS macro	|	note	|
|	---	|	---	|
|	DFHAFCD	|		|
|	DFHEIEND	|		|
|	DFHEIENT	|		|
|	DFHEIRET	|		|
|	DFHEISTG	|		|
|	DFHSYS	|		|

* System Macro
** 아래는 OFASM 내부적으로 지원하는 매크로들이다.

|	System macro	| note |
| --- | --- |
|	AMODESW	| |
|	ASMDREG	| |
|	DCBE	| |
|	SPLEVEL	| |
|	SYSSTATE	| |
|	TIMEUSED	| |
|	YREGS	| |


### 어셈블러 소스를 수정해야하는 상황

//에러 메세지에 따른 대처법

## 프로젝트 진행 중 발생하는 문제들

