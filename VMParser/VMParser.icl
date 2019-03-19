module VMParser

import StdEnv
import StdFile
import Directory

/*
*	Parse a "push constant" command:
*	1. constant = extract the constant field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
*	3. writes the command into file.
*
*/
parsePushConstant:: String *f -> Bool | FileSystem f  
parsePushConstant pushstr w
# constant = toString (drop (length [char \\ char <-: "push constant "]) [char \\ char <-: pushstr])
# instruction = "@" +++ constant +++ "\nD=A\n@0\nM=M+1\nA=M\nM=D\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= ok_close

/*
*	Parse a "add" command:
*	1. instruction = the Hack machine code for 'add' instruction.
*	2. writes the command into file
*
*/
parseAddCommand:: String *f -> Bool | FileSystem f  
parseAddCommand pushstr w
# instruction = "D=M\nA=A-1\nD=D+M\nM=D\n"
# (ok_open,file ,w) = fopen "out.asm" FAppendText w
| not ok_open = abort "failed to open file"
# file = fwrites instruction file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close"
= ok_close


Start w
# (dir,w) = getDirectoryContents (RelativePath []) w
= parseAddCommand "akkuna mattadda" w



