//Initial SP
@255
D=A
@0
M=D
M=M+1

//bootstrap
@Sys.init.returnAdd
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Sys.init.returnAdd
0;JMP
(Sys.init.returnAdd)//bootstrap-end

(Sys.init)
@0
D=A
@Sys.init.End
D;JEQ
(Sys.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.init.Loop
D=D-1;JNE
(Sys.init.End)
//push instruction
@4
D=A
@SP
A=M
M=D
@SP
M=M+1

@merged.vm.Main.fibonacciReturnAddress
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
@SP
D=M
@-4
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Main.fibonacci
0;JMP
(merged.vm.Main.fibonacci.ReturnAddress)
(merged.vm.WHILE)
@merged.vm.WHILE              // Loop infinitely

0;JMP
(Main.fibonacci)
@0
D=A
@Main.fibonacci.End
D;JEQ
(Main.fibonacci.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Main.fibonacci.Loop
D=D-1;JNE
(Main.fibonacci.End)
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

//push instruction
@2
D=A
@SP
A=M
M=D
@SP
M=M+1

// lt command:
@SP
A=M-1
D=M
A=A-1
D=M-D
@IF_TRUE9
D;JLT
D=0
@SP
A=M-1
A=A-1
M=D
@IF_FALSE9
0;JMP
(IF_TRUE9)
D=-1
@SP
A=M-1
A=A-1
M=D
(IF_FALSE9)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@merged.vm.IF_TRUE

D;JNE
@merged.vm.IF_FALSE

0;JMP
(merged.vm.IF_TRUE)
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
(merged.vm.IF_FALSE)
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

//push instruction
@2
D=A
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

@merged.vm.Main.fibonacciReturnAddress
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
@SP
D=M
@-4
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Main.fibonacci
0;JMP
(merged.vm.Main.fibonacci.ReturnAddress)
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

//push instruction
@1
D=A
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

@merged.vm.Main.fibonacciReturnAddress
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
@SP
D=M
@-4
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Main.fibonacci
0;JMP
(merged.vm.Main.fibonacci.ReturnAddress)
//add instruction
A=M-1
D=M
A=A-1
D=M+D
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
