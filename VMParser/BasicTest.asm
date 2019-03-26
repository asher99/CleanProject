//Initial SP
@255
D=A
@0
M=D
M=M+1

//push instruction
@10
D=A
@SP
A=M
M=D
@SP
M=M+1

//pop local instruction
@0

D=A
@LCL
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//push instruction
@21
D=A
@SP
A=M
M=D
@SP
M=M+1

//push instruction
@22
D=A
@SP
A=M
M=D
@SP
M=M+1

//pop argument instruction
@2

D=A
@ARG
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//pop argument instruction
@1

D=A
@ARG
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//push instruction
@36
D=A
@SP
A=M
M=D
@SP
M=M+1

//pop this instruction
@6

D=A
@THIS
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//push instruction
@42
D=A
@SP
A=M
M=D
@SP
M=M+1

//push instruction
@45
D=A
@SP
A=M
M=D
@SP
M=M+1

//pop that instruction
@5

D=A
@THIS
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//pop that instruction
@2

D=A
@THIS
A=M+D
D=A
@13
M=D
@SP
A=M-1
D=M
@13
A=M
M=D
@SP
M=M-1

//push instruction
@510
D=A
@SP
A=M
M=D
@SP
M=M+1

//pop temp instruction
@SP
A=M-1
D=M
@1
M=D
@SP
M=M-1

//push local instruction
@0

D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//push that instruction
@5

D=A
@THAT
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//add instruction
A=M-1
D=M
A=A-1
D=M+D
M=D
@0
M=M-1

//push argument instruction
@1

D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//sub instruction
A=M-1
D=M
A=A-1
D=M-D
M=D
@0
M=M-1

//push this instruction
@6

D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//push this instruction
@6

D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//add instruction
A=M-1
D=M
A=A-1
D=M+D
M=D
@0
M=M-1

//sub instruction
A=M-1
D=M
A=A-1
D=M-D
M=D
@0
M=M-1

//push temp instruction
@6

D=A
@TEMP
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

//add instruction
A=M-1
D=M
A=A-1
D=M+D
M=D
@0
M=M-1

