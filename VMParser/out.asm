//Initial SP
@255
D=A
@0
M=D
M=M+1
//push instruction
@7
D=A
@0
A=M
M=D
@0
M=M+1

//push instruction
@8
D=A
@0
A=M
M=D
@0
M=M+1

//add instruction
A=M-1
D=M
A=A-1
D=M+D
M=D
@0
M=M-1

