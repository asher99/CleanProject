implementation module Parsing
import StdEnv
import StdFile

parse:: [String] *f-> (Bool,*f) | FileSystem f
parse [] w = (True,w)
parse [x:xs] w
# (ok,w) = parseLine x w
| not ok = abort ("failed to parse line " +++ x +++ "\n")
= parse xs w

parseLine:: String *f-> (Bool,*f) | FileSystem f 
parseLine line w
| line == "\n" || line == "" = (True,w)							// Empty line
| line % (0,1) == "//" = (True,w)								// Comment
| line % (0,12) == "push constant" = parsePushConstant line w	// 'push constant #'
| line % (0,2) == "add" = parseAddCommand line w				// 'add'
| otherwise = (False,w)

/*
*	Parse a "push constant" command:
*	1. constant = extract the constant field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushConstant:: String *f -> (Bool,*f) | FileSystem f  
parsePushConstant pushstr w
# constant = toString (drop (length [char \\ char <-: "push constant "]) [char \\ char <-: pushstr])
# instruction = "//push instruction\n@" +++ constant +++ "D=A\n@0\nM=M+1\nA=M\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "add" command:
*	1. instruction = the Hack machine code for 'add' instruction.
*	2. writes the command into file
*
*/
parseAddCommand:: String *f -> (Bool,*f) | FileSystem f  
parseAddCommand addstr w
# instruction = "//add instruction\nD=M\nA=A-1\nD=D+M\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)