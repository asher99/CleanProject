@0		// Initialization
M=A

@7		// Push 7
D=A
@0
M=M+1
A=M
M=D

@8		// Push 8
D=A
@0
M=M+1
A=M
M=D
	
D=M		// Add
A=A-1
D=D+M
M=D