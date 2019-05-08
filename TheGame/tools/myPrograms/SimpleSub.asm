@0		// Initialization
M=A

@10		// Push 7
D=A
@0
M=M+1
A=M
M=D

@2		// Push 8
D=A
@0
M=M+1
A=M
M=D
	
D=M		// Add
A=A-1
D=M-D
M=D