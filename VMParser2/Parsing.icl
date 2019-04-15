implementation module Parsing
import StdEnv
import StdFile

/*
*	Split
*/
split :: String -> [String]
split s = scrape (fromString s)
where
    scrape :: [Char] -> [String]
    scrape [] = []
    scrape cs=:[c:_]
    | isSpace c = scrape (dropWhile isSpace cs)
    | otherwise = [toString word:scrape rest]
    where
        (word,rest) = span (not o isSpace) cs

/*
* Recursive parsing of the file. Parsing line by line untill list of lines is empty.
*/
parse:: [String] String Int *f -> (Bool,*f) | FileSystem f
parse [] filename num w = (True,w)
parse [x:xs] filename num w
# (ok,w) = parseLine x filename num w
| not ok = abort ("failed to parse line " +++ x +++ "\n")
= parse xs filename (num+1) w

/*
* Parse line. Uses a 'switch-case' template to prefrom the right parsing method.
*/
parseLine:: String String Int *f-> (Bool,*f) | FileSystem f 
parseLine line filename num w
| line == "\n" || line == "" = (True,w)							// Empty line
| line % (0,1) == "//" = (True,w)								// Comment
																// push commands					
| line % (0,12) == "push constant" = parsePushConstant line w	//  push constant #
| line % (0,10) == "push static" = parsePushStatic line w		//  push static #
| line % (0,12) == "push argument" = parsePushArgument line w	//  push argument #
| line % (0,9) == "push local" = parsePushLocal line w			//  push local #
| line % (0,8) == "push this" = parsePushThis line w			//  push this #
| line % (0,8) == "push that" = parsePushThat line w			//  push that #
| line % (0,8) == "push temp" = parsePushTemp line w			//  push temp #

																// pop commands
| line % (0,9) == "pop static" = parsePopStatic line w			//  pop static #
| line % (0,11) == "pop argument" = parsePopArgument line w		//  pop argument #
| line % (0,8) == "pop local" = parsePopLocal line w			//  pop local #
| line % (0,7) == "pop this" = parsePopThis line w				//  pop this #
| line % (0,7) == "pop that" = parsePopThat line w				//  pop that #
| line % (0,7) == "pop temp" = parsePopTemp line w				//  pop temp #

																// pointer commands
| line % (0,13) == "push pointer 0" = parsePushPointer0 line w	//  push pointer 0
| line % (0,12) == "pop pointer 0" = parsePopPointer0 line w	//  pop pointer 0
| line % (0,13) == "push pointer 1" = parsePushPointer1 line w	//  push pointer 1
| line % (0,12) == "pop pointer 1" = parsePopPointer1 line w	//  pop pointer 1

| line % (0,4) == "label" = parseLabel line filename w			// label c
| line % (0,3) == "goto" = parseGoto line filename w			// goto c
| line % (0,6) == "if-goto" = parseIfGoto line filename w		// if-goto c

| line % (0,3) == "call" = parseCall line filename w			// call f n
| line % (0,7) == "function" = parseFunction line filename w	// function f k
| line % (0,5) == "return" = parseReturn line filename w		// return

| line % (0,2) == "add" = parseAddCommand line w				//  add
| line % (0,2) == "sub" = parseSubCommand line w				//  sub
| line % (0,2) == "neg" = parseNegCommand line w				//  neg
| line % (0,2) == "and" = parseAndCommand line w				//  and
| line % (0,1) == "or"  = parseOrCommand  line w				//  or
| line % (0,2) == "not" = parseNotCommand line w				//  not
| line % (0,1) == "eq"  = parseEQCommand  line num w			//  eq
| line % (0,1) == "gt"  = parseGTCommand  line num w			//  gt
| line % (0,1) == "lt"  = parseLTCommand  line num w			//  lt
| otherwise = (False,w)

// ********************  bootstrap *****************************//



// ********************  push *****************************//

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
# instruction = "//push instruction\n@" +++ constant +++ "D=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "push static" command:
*	1. static = extract the static field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushStatic:: String *f -> (Bool,*f) | FileSystem f  
parsePushStatic pushstr w
# offset = toString (drop (length [char \\ char <-: "push static "]) [char \\ char <-: pushstr])
# offset = offset % (0, (size offset)-2)
# addr = (toInt offset)+16
# addstr = toString addr
# instruction = "//push static instruction\n@" +++ addstr +++ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "push argument" command:
*	1. argument = extract the argument field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushArgument:: String *f -> (Bool,*f) | FileSystem f  
parsePushArgument pushstr w
# offset = toString (drop (length [char \\ char <-: "push argument "]) [char \\ char <-: pushstr])
# instruction = "//push argument instruction\n@" +++ offset +++ "\nD=A\n@ARG\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "push local" command:
*	1. local = extract the local field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushLocal :: String *f -> (Bool,*f) | FileSystem f  
parsePushLocal pushstr w
# offset = toString (drop (length [char \\ char <-: "push local "]) [char \\ char <-: pushstr])
# instruction = "//push local instruction\n@" +++ offset +++ "\nD=A\n@LCL\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "push this" command:
*	1. this = extract the this field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushThis :: String *f -> (Bool,*f) | FileSystem f  
parsePushThis pushstr w
# offset = toString (drop (length [char \\ char <-: "push this "]) [char \\ char <-: pushstr])
# instruction = "//push this instruction\n@" +++ offset +++ "\nD=A\n@THIS\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "push that" command:
*	1. this = extract the this field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushThat :: String *f -> (Bool,*f) | FileSystem f  
parsePushThat pushstr w
# offset = toString (drop (length [char \\ char <-: "push that "]) [char \\ char <-: pushstr])
# instruction = "//push that instruction\n@" +++ offset +++ "\nD=A\n@THAT\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "push temp" command:
*	1. temp = extract the temp field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushTemp :: String *f -> (Bool,*f) | FileSystem f  
parsePushTemp pushstr w
# offset = toString (drop (length [char \\ char <-: "push temp "]) [char \\ char <-: pushstr])
# offset = offset % (0, (size offset)-2)
# addr = (toInt offset)+5
# addstr = toString addr
# instruction = "//push temp instruction\n@" +++ addstr +++ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

parsePushPointer0:: String *f -> (Bool,*f) | FileSystem f  
parsePushPointer0 pushstr w
# instruction = "//push static instruction\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

parsePushPointer1:: String *f -> (Bool,*f) | FileSystem f  
parsePushPointer1 pushstr w
# instruction = "//push static instruction\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

// ********************  pop *****************************//

/*
*	Parse a "pop constant" command:
*	1. constant = extract the constant field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopConstant:: String *f -> (Bool,*f) | FileSystem f  
parsePopConstant popstr w
# constant = toString (drop (length [char \\ char <-: "pop constant "]) [char \\ char <-: popstr])
# instruction = "//pop instruction\n@" // TODO +++ constant +++ "D=A\n@0\nA=M\nM=D\n@0\nM=M+1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "pop static" command:
*	1. static = extract the static field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopStatic:: String *f -> (Bool,*f) | FileSystem f  
parsePopStatic popstr w
# offset = toString (drop (length [char \\ char <-: "pop static "]) [char \\ char <-: popstr])
# offset = offset % (0, (size offset)-2)
# addr = (toInt offset)+16
# addstr = toString addr
# instruction = "//pop static instruction\n@SP\nA=M-1\nD=M\n@"+++addstr+++"\nM=D\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "pop argument" command:
*	1. argument = extract the argument field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopArgument:: String *f -> (Bool,*f) | FileSystem f  
parsePopArgument popstr w
# offset = toString (drop (length [char \\ char <-: "pop argument "]) [char \\ char <-: popstr])
# instruction = "//pop argument instruction\n@"+++offset+++"\nD=A\n@ARG\nA=M+D\nD=A\n@13\nM=D\n@SP\nA=M-1\nD=M\n@13\nA=M\nM=D\n@SP\nM=M-1\n@13\nM=0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "pop local" command:
*	1. local = extract the local field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopLocal :: String *f -> (Bool,*f) | FileSystem f  
parsePopLocal popstr w
# offset = toString (drop (length [char \\ char <-: "pop local "]) [char \\ char <-: popstr])
# instruction = "//pop local instruction\n@"+++offset+++"\nD=A\n@LCL\nA=M+D\nD=A\n@13\nM=D\n@SP\nA=M-1\nD=M\n@13\nA=M\nM=D\n@SP\nM=M-1\n@13\nM=0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "pop this" command:
*	1. this = extract the this field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopThis :: String *f -> (Bool,*f) | FileSystem f  
parsePopThis popstr w
# offset = toString (drop (length [char \\ char <-: "pop this "]) [char \\ char <-: popstr])
# instruction = "//pop this instruction\n@"+++offset+++"\nD=A\n@THIS\nA=M+D\nD=A\n@13\nM=D\n@SP\nA=M-1\nD=M\n@13\nA=M\nM=D\n@SP\nM=M-1\n@13\nM=0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

/*
*	Parse a "pop that" command:
*	1. this = extract the this field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopThat :: String *f -> (Bool,*f) | FileSystem f  
parsePopThat popstr w
# offset = toString (drop (length [char \\ char <-: "pop that "]) [char \\ char <-: popstr])
# instruction = "//pop that instruction\n@"+++offset+++"\nD=A\n@THAT\nA=M+D\nD=A\n@13\nM=D\n@SP\nA=M-1\nD=M\n@13\nA=M\nM=D\n@SP\nM=M-1\n@13\nM=0\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)


/*
*	Parse a "pop temp" command:
*	1. temp = extract the temp field from the VM instruction.
*	2. instruction = the Hack machine code for 'pop' instruction.
*	3. writes the command into file.
*
*/
parsePopTemp :: String *f -> (Bool,*f) | FileSystem f  
parsePopTemp popstr w
# offset = toString (drop (length [char \\ char <-: "pop temp "]) [char \\ char <-: popstr])
# offset = offset % (0, (size offset)-2)
# addr = (toInt offset)+5
# addstr = toString addr
# instruction = "//pop temp instruction\n@SP\nA=M-1\nD=M\n@"+++addstr+++"\nM=D\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

parsePopPointer0:: String *f -> (Bool,*f) | FileSystem f  
parsePopPointer0 popstr w
# instruction = "//pop instruction\n@SP\nA=M-1\nD=M\n@THIS\nM=D\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

parsePopPointer1:: String *f -> (Bool,*f) | FileSystem f  
parsePopPointer1 popstr w
# instruction = "//pop instruction\n@SP\nA=M-1\nD=M\n@THAT\nM=D\n@SP\nM=M-1\n\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

// ********************  control flow *****************************//

//comment
parseLabel:: String String *f -> (Bool,*f) | FileSystem f  
parseLabel label filename w
# list = split label
# labelName = list!!1
# instruction = "(" +++ filename +++ "." +++ labelName +++ ")\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

//comment
parseGoto:: String String *f -> (Bool,*f) | FileSystem f  
parseGoto gotostr filename w
# label = toString (drop (length [char \\ char <-: "goto "]) [char \\ char <-: gotostr])
# instruction = "@" +++ filename +++ "." +++ label +++ "\n0;JMP\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

//comment
parseIfGoto:: String String *f -> (Bool,*f) | FileSystem f  
parseIfGoto gotostr filename w
# label = toString (drop (length [char \\ char <-: "if-goto "]) [char \\ char <-: gotostr])
# instruction = "@SP\nM=M-1\nA=M\nD=M\n" +++ "@" +++ filename +++ "." +++ label +++ "\nD;JNE\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

//comment
parseCall:: String String *f -> (Bool,*f) | FileSystem f  
parseCall linestr filename w
# list = split linestr
# newARG = toString ((toInt (list!!2) - 5))
# storestr = "D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
# instruction = "@" +++ filename +++ "." +++ list!!1 +++"ReturnAddress\n@LCL\n" +++ storestr +++ "@ARG\n" +++ storestr +++ "@THIS\n" +++ storestr +++ "@THAT\n@SP\nD=M\n@" +++ newARG +++ "\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n@" +++ list!!1 +++ "\n0;JMP\n(" +++ filename +++ "." +++ list!!1 +++ ".ReturnAddress)\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

//comment
parseFunction:: String String *f -> (Bool,*f) | FileSystem f  
parseFunction linestr filename w
# list = split linestr
# instruction = "(" +++ list!!1 +++ ")\n@" +++ list!!2 +++ "\nD=A\n@" +++ list!!1 +++ ".End\nD;JEQ\n(" +++ list!!1 +++ ".Loop)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@" +++ list!!1 +++ ".Loop\nD=D-1;JNE\n" +++ "(" +++ list!!1 +++ ".End)\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

//comment
parseReturn:: String String *f -> (Bool,*f) | FileSystem f  
parseReturn linestr filename w
# instruction = "@LCL\nD=M\n@5\nA=D-A\nD=M\n@13\nM=D\n@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nM=D\n@ARG\nD=M\n@SP\nM=D+1\n@LCL\nM=M-1\nA=M\nD=M\n@THAT\nM=D\n@LCL\nM=M-1\nA=M\nD=M\n@THIS\nM=D\n@LCL\nM=M-1\nA=M\nD=M\n@ARG\nM=D\n@LCL\nM=M-1\nA=M\nD=M\n@LCL\nM=D\n@13\nA=M\n0;JMP\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= (ok_close,w)

// ********************  ALU *****************************//


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