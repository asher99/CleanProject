/*
	Programming Language Principles, 2019 Course.
	Targil-1, by:
		Asher Alexander 	206195356
		Zvei Eliezer Nir	316525344

	Lab supervisor: Aviram Zilberman.
*/
module VMParser

import StdEnv
import StdFile
import Parsing
import FileOperations


Start w
// 1. Receive name of '.vm' file to parse.
# (io,w) = stdio w                                				// open stdio
# io = fwrites "Enter name of VM file (with '.vm'):\n" io    	// ask for name
# (name,io) = freadline io                        			// read in name
# name = name % (0, size name - 2)                			// remove \n from name
# (ok,w) = fclose io w                            			// close stdio
| not ok = abort "Couldn't close stdio"           			// abort in case of failure

// 2. Read the content of the file to list of string: each string is a line.
# (ok_read_open,inputfile,w) = fopen name FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w

// 3. Initial the output '.asm' file.
# (ok_open,file ,w) = fopen "out.asm" FWriteText w
| not ok_open = abort "Failed to open output file.\n"
//# file = fwrites "//Initial SP\n@0\nM=A\n\n" file
# file = fwrites "// BOOTSTRAP\n@256\nD=A\n@SP\nM=D\n@Sys.init.returnAdd\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nD=M\n@5\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n@Sys.init.returnAdd\n0;JMP\n(Sys.init.returnAdd)//bootstrap-end\n\n" file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close output file.\n"

// 4. Parse the content of the file.
= parse content name 1 w



