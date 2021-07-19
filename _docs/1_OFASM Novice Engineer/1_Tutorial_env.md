---
title: OFASM Tutorial Environment
category: OFASM Novice Engineer
order: 1
---

## 목차 
* 환경 구성
  * 설치
  * 설치 확인

---

## 환경 구성
### 설치

* 인스톨러를 이용한 설치

    인스톨러를 이용한 설치는 OFASM Installer Guide 를 참고한다.

* 바이너리를 이용한 설치

1. OFASM Home 디렉토리 생성
   
        $ mkdir $HOME/OFASM

2. tar 로 $HOME/OFASM 에 압축해제

        $ tar -xvf ofasm_v4_r1095_master_linux_x86.tar.gz -C $HOME/OFASM

3. 환경 설정 profile에 아래 내용 입력 후 저장 (OFASM 에는 Release 바이너리와 Debug 바이너리가 있다. 본 절에서는 Debug 바이너리를 사용한다.)

        $ vi ~/.bash_profile

        export OFASM_HOME=$HOME/OFASM/Debug
        export PATH=$OFAMS_HOME/bin
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OFASM_HOME/lib
        export OFASM_MACLIB=$OFASM_HOME/maclib/ofmac:$OFASM_HOME/maclib/

4. profile 적용

        $ source ~/.bash_profile

### 설치 확인

1. ofasm help 메시지 확인.

        $ ofasm --help
        OpenFrame Assembler 4

        Usage : ofasm [options] input_file
                ofasm input_file [[-x] <params...>]
                ofasm input_file [( [-f] <value> | [--entry] <value> | [-r] <reg_no> <value> ) [-x] <params...>]

        Options:
          -E : only preprocess
          -S : only assembly
          -H, --help : help information
          -g : produce debugging information to be used by the ofasm_debugger
          -x <params...> : execute instantly assembler program in command line by the user-specified parameters
                          parameters can be used only HEX value
          -f <filepath> : the json-formatted file that defined parameter information
                          this option can only be used with -x option
          -r <reg_no> <value> : specify register value before running
                                this option can only be used with -x option
          --entry <entryname> : specify the entryname to be run
                                this option can only be used with -x option
          --version : show ofasm version
          --list : show supported instruction
          --save-punch : save punch output file
          --enable-cics : enable cics preprocessing
          --enable-spm : enable structured programming macro
          --debug : show debug message
          --license : show license information

2. ofasm 버전 확인.

        $ ofasm --version
        OpenFrame Assembler 4
        Revision: 1099
        CommitID: c449ce7