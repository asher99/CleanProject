// BOOTSTRAP
@256
D=A
@SP
M=D
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

//Function Instruction
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
@6
D=A
@SP
A=M
M=D
@SP
M=M+1

//push instruction
@8
D=A
@SP
A=M
M=D
@SP
M=M+1

//Call Instruction
@st.vm.4.Class1.set.ReturnAddress
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
@2
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Class1.set
0;JMP
(st.vm.4.Class1.set.ReturnAddress)
//pop temp instruction
@SP
A=M-1
D=M
@5
M=D
@SP
M=M-1

//push instruction
@23
D=A
@SP
A=M
M=D
@SP
M=M+1

//push instruction
@15
D=A
@SP
A=M
M=D
@SP
M=M+1

//Call Instruction
@st.vm.8.Class2.set.ReturnAddress
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
@2
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Class2.set
0;JMP
(st.vm.8.Class2.set.ReturnAddress)
//pop temp instruction
@SP
A=M-1
D=M
@5
M=D
@SP
M=M-1

//Call Instruction
@st.vm.10.Class1.get.ReturnAddress
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
@0
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Class1.get
0;JMP
(st.vm.10.Class1.get.ReturnAddress)
//Call Instruction
@st.vm.11.Class2.get.ReturnAddress
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
@0
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Class2.get
0;JMP
(st.vm.11.Class2.get.ReturnAddress)
(st.vm.WHILE)
//Goto Instruction
@st.vm.WHILE

0;JMP
//Function Instruction
(Class1.set)
@0
D=A
@Class1.set.End
D;JEQ
(Class1.set.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Class1.set.Loop
D=D-1;JNE
(Class1.set.End)
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

//pop static instruction
@SP
A=M-1
D=M
@st.vm.0
M=D
@SP
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

//pop static instruction
@SP
A=M-1
D=M
@st.vm.1
M=D
@SP
M=M-1

//push instruction
@0
D=A
@SP
A=M
M=D
@SP
M=M+1

//Ret Instruction
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
//Function Instruction
(Class1.get)
@0
D=A
@Class1.get.End
D;JEQ
(Class1.get.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Class1.get.Loop
D=D-1;JNE
(Class1.get.End)
//push static instruction
@st.vm.0
D=M
@SP
A=M
M=D
@SP
M=M+1

//push static instruction
@st.vm.1
D=M
@SP
A=M
M=D
@SP
M=M+1

//sub instruction
@SP
A=M-1
D=M
A=A-1
D=M-D
M=D
@0
M=M-1

//Function Instruction
(Class2.set)
@0
D=A
@Class2.set.End
D;JEQ
(Class2.set.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Class2.set.Loop
D=D-1;JNE
(Class2.set.End)
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

//pop static instruction
@SP
A=M-1
D=M
@st.vm.0
M=D
@SP
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

//pop static instruction
@SP
A=M-1
D=M
@st.vm.1
M=D
@SP
M=M-1

//push instruction
@0
D=A
@SP
A=M
M=D
@SP
M=M+1

//Ret Instruction
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
//Function Instruction
(Class2.get)
@0
D=A
@Class2.get.End
D;JEQ
(Class2.get.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Class2.get.Loop
D=D-1;JNE
(Class2.get.End)
//push static instruction
@st.vm.0
D=M
@SP
A=M
M=D
@SP
M=M+1

//push static instruction
@st.vm.1
D=M
@SP
A=M
M=D
@SP
M=M+1

//sub instruction
@SP
A=M-1
D=M
A=A-1
D=M-D
M=D
@0
M=M-1

//Ret Instruction
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
//Ret Instruction
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
