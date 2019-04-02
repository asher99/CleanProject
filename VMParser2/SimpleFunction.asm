//Initial SP
@255
D=A
@0
M=D
M=M+1

(SimpleFunction.test)
@2
D=A
@SimpleFunction.test.End
D;JEQ
(SimpleFunction.test.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SimpleFunction.test.Loop
D=D-1;JNE
(SimpleFunction.test.End)
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

//push local instruction
@1

D=A
@LCL
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

//not instruction
A=M-1
M=!M
D=M
@0

//push argument instruction
@0

D=A
@ARG
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

@LCL
D=M
@5
A=D-A
D=M
@13
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@LCL
M=M-1
A=M
D=M
@THAT
M=D
@LCL
M=M-1
A=M
D=M
@THIS
M=D
@LCL
M=M-1
A=M
D=M
@ARG
M=D
@LCL
M=M-1
A=M
D=M
@LCL
M=D
@13
A=M
0;JMP
