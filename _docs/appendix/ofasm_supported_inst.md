---
title: OFASM이 지원하는 명령어
category: Appendix
order: 1
---

## OFASM 지원 명령어

OFASM이 지원하는 명령어는 아래 리스트를 참고한다.

### Preprocessing (Conditional assembly) Instruction

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

### Inner macro instruction

| Instruction Name | OFASM testcase | note |
| --- | --- | --- |
| COPY  | X | |
| MEXIT | X | |

### Assembler Instruction

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

### Machine Instruction

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


### Extended Mnemonic Instruction

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

### SVC Macro
아래는 현재 OFASM에서 지원하고 있는 SVC 및 그와 연관된 매크로들이다.

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

### CICS 
아래는 OFASM 내부적으로 지원 하거나, 내부 동작을 거친 후에 cics 관련 함수를 호출하는 CICS 명령어들이다.

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

아래는 OFASM 내부적으로 지원하는 CICS 관련 카피북/매크로이다.

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

### Internal Macro
아래는 OFASM 내부적으로 지원하는 매크로들이다.

|	System macro	| note |
| --- | --- |
|	AMODESW	| |
|	ASMDREG	| |
|	DCBE	| |
|	SPLEVEL	| |
|	SYSSTATE	| |
|	TIMEUSED	| |
|	YREGS	| |