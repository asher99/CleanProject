module VMParser

import StdEnv
import StdFile
import Directory

/*
*	Parse a "push constant" command:
*	1. constant = extract the constant field from the VM instruction.
*	2. instruction = the Hack machine code for 'push' instruction.
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



Start w
# (dir,w) = getDirectoryContents (RelativePath []) w
= parsePushConstant "push constant 256" w



