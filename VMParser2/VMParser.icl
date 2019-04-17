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
import FileManipulation


printList:: [String] *File -> *File
printList [] io = io
printList [x] io
# io = fwrites x io
# io = fwrites ".\n" io
= printList [] io
printList [x:xs] io
# io = fwrites x io
# io = fwrites ", " io
= printList xs io

moreThanOneFile:: [String] Int -> Bool
moreThanOneFile [] num
| num > 1 = True
| otherwise = False
moreThanOneFile [x:xs] num
# num = num + 1
= moreThanOneFile xs num 
 
parseNoBootstrap:: [String] *File *f -> Bool| FileSystem f
parseNoBootstrap filesList io w
// 3. Initial the output '.asm' file.
# (ok_open,file ,w) = fopen "out.asm" FWriteText w
| not ok_open = abort "Failed to open output file.\n"
# file = fwrites "// initial SP\n@256\nD=A\n@SP\nM=D\n\n" file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close output file.\n"
// 4. Parse the content of the file.
# (success,w) = parseMultipleFiles filesList w
| not success = abort "Parsing failed :-(\n"
# io = fwrites "Parsing completed. No bootstrap was added since there is only one file.\nlook for the \"out.asm\" file in the working directory.\n"io    	// ask for name
# (ok,w) = fclose io w                            			// close stdio
| not ok = abort "Couldn't close stdio"           			// abort in case of failure
= True

parseWithBootstrap:: [String] *File *f -> Bool | FileSystem f
parseWithBootstrap filesList io w
// 3. Initial the output '.asm' file.
# (ok_open,file ,w) = fopen "out.asm" FWriteText w
| not ok_open = abort "Failed to open output file.\n"
# file = fwrites "// BOOTSTRAP\n@256\nD=A\n@SP\nM=D\n@Sys.init.returnAdd\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nD=M\n@5\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n@Sys.init\n0;JMP\n(Sys.init.returnAdd)//bootstrap-end\n\n" file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close output file.\n"
// 4. Parse the content of the file.
# (success,w) = parseMultipleFiles filesList w
| not success = abort "Parsing failed :-(\n"
# io = fwrites "Parsing completed, look for the \"out.asm\" file in the working directory.\n"io    	// ask for name
# (ok,w) = fclose io w                            			// close stdio
| not ok = abort "Couldn't close stdio"           			// abort in case of failure
= True



Start w
// 1. Receive name of '.vm' file to parse.
# (io,w) = stdio w                                				// open stdio
# io = fwrites "VM 2 HACK PARSER by Asher Alexander & Zvei Eliezer Nir\n" io    	// ask for name

// 2. Import the content of InputFiles directory
# (dir,w) = getDirectoryContents (RelativePath [PathDown "InputFiles"]) w
# filesList = getVmFiles (getNamesOfFilesInDirectory (getEntriesList dir))
# io = fwrites "The parser found the following .VM files: " io
# io = printList filesList io

// 3. 
| moreThanOneFile filesList 0 = (parseWithBootstrap filesList io w)
| otherwise = (parseNoBootstrap filesList io w)

