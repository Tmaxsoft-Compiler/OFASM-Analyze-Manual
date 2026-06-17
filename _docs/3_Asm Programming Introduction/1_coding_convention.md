---
title: Assembler 코딩 규칙
category: ASM Programming Introduction
order: 1
---

## 목차

- [목차](#목차)
- [Field boundaries](#field-boundaries)
  - [Statement field](#statement-field)
  - [Continuation-indicator field](#continuation-indicator-field)
  - [Identification-sequence field](#identification-sequence-field)

----

## Field boundaries
하나의 Assembly language statement 는 80 자의 record 또는 열을 차지한다.
80 자를 초과하는 경우 Continuation line 를 사용해야 하며, 각 행은 세 개의 기본 필드로 나뉘어진다. 아래는 각 항목에 대한 그림이다.

![코딩규칙]({{site.baseurl}}/attach/field_boundaries.png)

### Statement field
Statement field 는 1 ~ 71 열까지 이다. Instruction 및 주석문은 Statement field 에 기재되어야 한다. Statement field 는 아래와 같이 Name field, Operation field, Operand field, Comment 로 이루어져 있다.

- Name Field : <br> 
Symbol, Label, Name Entry 등의 다양한 단어로 불린다 어셈블리 코드는 각각의 줄이 연속된 주소를 갖고 있다. 특정 라인의 주소를 명시적(Explicit)으로 가리키는 방법이 있으나, 모두 이렇게 작성하기는 불편하다. 이를 암시적(Implicit)으로 단어, 숫자, 특수기호 등으로 간단하게 표현하기 위해 이 공간을 이용한다. IBM에서는 최대 62 길이까지 지원된다. 이 Field는 Option이므로 하나 이상의 “공백”으로 무시될 수 있다.

 - Operation Field : <br>
Name Field에서 하나 이상의 “공백” 이후에 입력되는 단어는 Operation으로 인식된다. Instruction이라는 용어는 Operation 와 Operand 을 합쳐서 일컫는 용어이다. 각 Operation들은 고유의 Hexadecimal 값을 갖고 있으며, OPCODE로 불린다.

- Operand Field : <br>
Operation Field에서 하나 이상의 “공백” 이후에 입력되는 숫자, 문자 그리고 “,” 의 조합으로 구성된다. 이 필드의 값들은 Operation마다 요구하는 값이 다르며, Name field에 작성된 단어가 포함되거나, 레지스터 등이 입력될 수 있다. 각각의 Operand들은 “,”를 이용하여 구분하며, 절대로 사이에 “공백”이 들어갈 수 없다. 이 필드는 Operand에 따라 사용하지 않을 수도 있다.

 - Comment : <br>
Operand Field에서 하나 이상의 “공백” 다음에 입력되는 공간으로, 주석을 뜻한다. 71 column까지 작성할 수 있다.

### Continuation-indicator field
Continuation indicator field 는 72 열에 기술되야 한다. 이 열에서 공백이 아닌 문자 여야 하며 다음 줄에서 계속됨을 나타낸다. 계속된 줄은 16 열에서 시작해야 하며 이를 Continuation lines 이라 한다. 작성 규칙은 아래와 같다.

1. Continuation indicator field (72열)에 공백이 아닌 문자를 입력한다. 이 공백이 아닌 문자는 Instruction 일부가 아니어야 한다. 하나 이상의 연속 라인이 필요한 경우 계속되는 각 행의 72열에 공백이 아닌 문자가 있어야 한다.

2. Continuation lines(열 16)에서 시작하여 다음 줄에서 명령문을 계속한다.  Continuation lines의 왼쪽은 공백이어야 한다.

### Identification-sequence field
73 ~ 80 열까지는 식별 문자나 시퀀스 번호로서 사용된다. 주석문과 동일하게 취급된다.


