implementation module Parsing
import StdEnv
import StdFile

/*
* Recursive parsing of the file. Parsing line by line untill list of lines is empty.
*/
parse:: [String] Int *f -> (Bool,*f) | FileSystem f
parse [] num w = (True,w)
parse [x:xs] num w
# (ok,w) = parseLine x num w
| not ok = abort ("failed to parse line " +++ x +++ "\n")
//# num = inc num
= parse xs (num+1) w

/*
* Parse line. Uses a 'switch-case' template to prefrom the right parsing method.
*/
parseLine:: String Int *f-> (Bool,*f) | FileSystem f 
parseLine line num w
| line == "\n" || line == "" = (True,w)							// Empty line
| line % (0,1) == "//" = (True,w)								// Comment
| line % (0,12) == "push constant" = parsePushConstant line w	//  push constant #
| line % (0,2) == "add" = parseAddCommand line w				//  add
| line % (0,2) == "sub" = parseSubCommand line w				//  sub
| line % (0,2) == "neg" = parseNegCommand line w				//  neg
| line % (0,2) == "and" = parseAndCommand line w				//  and
| line % (0,1) == "or"  = parseOrCommand  line w				//  or
| line % (0,2) == "not" = parseNotCommand line w				//  not
| line % (0,1) == "eq"  = parseEQCommand  line num w			//  eq
| line % (0,1) == "gt"  = parseGTCommand  line num w			//  gt
| line % (0,1) == "lt"  = parseLTCommand  line num w			//  gt
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
# instruction = "//push instruction\n@" +++ constant +++ "D=A\n@0\nA=M\nM=D\n@0\nM=M+1\n\n"
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
//# instruction = "//add instruction\nA=M\nD=M\nA=A-1\nD=M+D\nM=D\n\n"
# instruction = "//add instruction\nA=M-1\nD=M\nA=A-1\nD=M+D\nM=D\n@0\nM=M-1\n\n"
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
# instruction = "//sub instruction\nA=M-1\nD=M\nA=A-1\nD=M-D\nM=D\n@0\nM=M-1\n\n"
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
# instruction = "//neg instruction\nA=M-1\nM=-M\nD=M\n@0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "and" command:
*	1. instruction = the Hack machine code for 'and' instruction.
*	2. writes the command into file
*
*/
parseAndCommand:: String *f -> (Bool,*f) | FileSystem f  
parseAndCommand andstr w
# instruction = "//and instruction\nA=M-1\nD=M\nA=A-1\nD=D&M\nM=D\n@0\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "or" command:
*	1. instruction = the Hack machine code for 'or' instruction.
*	2. writes the command into file
*
*/
parseOrCommand:: String *f -> (Bool,*f) | FileSystem f  
parseOrCommand orstr w
# instruction = "//or instruction\nA=M-1\nD=M\nA=A-1\nD=D|M\nM=D\n@0\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "not" command:
*	1. instruction = the Hack machine code for 'not' instruction.
*	2. writes the command into file
*
*/
parseNotCommand:: String *f -> (Bool,*f) | FileSystem f  
parseNotCommand notstr w
# instruction = "//not instruction\nA=M-1\nM=!M\nD=M\n@0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "eq" command:
*	1. instruction = the Hack machine code for 'eq' instruction.
*	2. writes the command into file
*	3. the line number in the original vm is kept to generate the appropiate labels.
*
*/
parseEQCommand:: String Int *f -> (Bool,*f) | FileSystem f  
parseEQCommand eqstr num w
# numOfLine = toString num 
# instruction = "// eq command:\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"+++ numOfLine +++"\nD;JEQ\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"+++ numOfLine +++"\n0;JMP\n(IF_TRUE"+++ numOfLine +++")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"+++ numOfLine +++")\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "gt" command:
*	1. instruction = the Hack machine code for 'gt' instruction.
*	2. writes the command into file
*	3. the line number in the original vm is kept to generate the appropiate labels.
*
*/
parseGTCommand:: String Int *f -> (Bool,*f) | FileSystem f  
parseGTCommand gtstr num w
# numOfLine = toString num 
# instruction = "// gt command:\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"+++ numOfLine +++"\nD;JGT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"+++ numOfLine +++"\n0;JMP\n(IF_TRUE"+++ numOfLine +++")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"+++ numOfLine +++")\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "lt" command:
*	1. instruction = the Hack machine code for 'lt' instruction.
*	2. writes the command into file
*	3. the line number in the original vm is kept to generate the appropiate labels.
*
*/
parseLTCommand:: String Int *f -> (Bool,*f) | FileSystem f  
parseLTCommand ltstr num w
# numOfLine = toString num 
# instruction = "// lt command:\n@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE"+++ numOfLine +++"\nD;JLT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE"+++ numOfLine +++"\n0;JMP\n(IF_TRUE"+++ numOfLine +++")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE"+++ numOfLine +++")\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)