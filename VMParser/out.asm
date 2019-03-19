//Initial SP
@0
M=A

//push instruction
@7
D=A
@0
M=M+1
A=M
M=D

//push instruction
@8
D=A
@0
M=M+1
A=M
M=D

//add instruction
D=M
A=A-1
D=M+D
M=D

