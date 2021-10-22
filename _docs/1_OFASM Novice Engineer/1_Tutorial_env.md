---
title: 환경 구성
category: OFASM Novice Engineer
order: 1
---

## 목차 
- [목차](#목차)
- [환경 구성](#환경-구성)
  - [설치](#설치)
  - [설치 확인](#설치-확인)
  - [매크로 설정](#매크로-설정)
    - [MVC3.8 매크로 내려 받기](#MVC3-8-매크로-내려-받기)
    - [매크로 경로 설정](#매크로-경로-설정)
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

### 매크로 설정
#### MVC3.8 매크로 내려 받기

대부분의 HLASM 자산은 IBM의 시스템 매크로를 사용하여 작성되어 있다. IBM 시스템 매크로의 무료 버전인 MVS3.8 매크로를 사용하면 대부분의 자산들은 컴파일이 가능하다.

MVS3.8 매크로는 편의상 38 매크로라 부르고 있으며 아래 링크에서 다운 받을 수 있다.

http://www.mainframe.eu/mvs38/

https://github.com/moshix/MVS38j.SYS1.MACLIB

라이센스 정보가 필요하다면 두 번째 링크에서 참조할 수 있다.

아래는 OFASM 팀 내부적으로 관리하고 있는 github 주소이다.

https://github.com/Tmaxsoft-Compiler/OFASM-MAC-LIB

#### 매크로 경로 설정

다운 받은 매크로를 적절한 경로에 모아두셨다면, 환경 변수를 설정해야 한다.

`OFASM_MACLIB` 환경 변수를 통해 매크로 디렉토리의 경로를 설정한다.

```
export OFASM_MACLIB=/home/oframe7/ofasm/maclib/ofmac:/home/oframe7/ofasm/maclib/38mac
```

> :warning: ofmac 경로는 다른 매크로 경로보다 항상 앞서야한다.