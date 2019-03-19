implementation module Parsing
import StdEnv
import StdFile

/*
* Recursive parsing of the file. Parsing line by line untill list of lines is empty.
*/
parse:: [String] *f-> (Bool,*f) | FileSystem f
parse [] w = (True,w)
parse [x:xs] w
# (ok,w) = parseLine x w
| not ok = abort ("failed to parse line " +++ x +++ "\n")
= parse xs w

/*
* Parse line. Uses a 'switch-case' template to prefrom the right parsing method.
*/
parseLine:: String *f-> (Bool,*f) | FileSystem f 
parseLine line w
| line == "\n" || line == "" = (True,w)							// Empty line
| line % (0,1) == "//" = (True,w)								// Comment
| line % (0,12) == "push constant" = parsePushConstant line w	// 'push constant #'
| line % (0,2) == "add" = parseAddCommand line w				// 'add'
| line % (0,2) == "sub" = parseSubCommand line w
| line % (0,2) == "neg" = parseNegCommand line w
| line % (0,2) == "and" = parseAndCommand line w
| line % (0,1) == "or" = parseOrCommand line w
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
# instruction = "//add instruction\nD=M\nA=A-1\nD=M+D\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "sub" command:
*	1. instruction = the Hack machine code for 'sub' instruction.
*	2. writes the command into file
*
*/
parseSubCommand:: String *f -> (Bool,*f) | FileSystem f  
parseSubCommand substr w
# instruction = "//sub instruction\nD=M\nA=A-1\nD=M-D\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "neg" command:
*	1. instruction = the Hack machine code for 'neg' instruction.
*	2. writes the command into file
*
*/
parseNegCommand:: String *f -> (Bool,*f) | FileSystem f  
parseNegCommand negstr w
# instruction = "//neg instruction\nM=-M\nD=M\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "and" command:
*	1. instruction = the Hack machine code for 'add' instruction.
*	2. writes the command into file
*
*/
parseAndCommand:: String *f -> (Bool,*f) | FileSystem f  
parseAndCommand andstr w
# instruction = "//and instruction\nD=M\nA=A-1\nD=D&M\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "or" command:
*	1. instruction = the Hack machine code for 'add' instruction.
*	2. writes the command into file
*
*/
parseOrCommand:: String *f -> (Bool,*f) | FileSystem f  
parseOrCommand andstr w
# instruction = "//or instruction\nD=M\nA=A-1\nD=D|M\nM=D\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)