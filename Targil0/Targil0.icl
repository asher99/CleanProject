/*
Targil 0 using Clean programming language,
Programming Languages Principles course 2019.

By Asher Alexander		206195356	
&& Zvei Eliezer Nir		316525344

*/

module Targil0

import StdEnv
import StdFile
import FileManipulation


writeToAsm:: [String] *f -> Bool| FileSystem f
writeToAsm []  w = True
writeToAsm [x:xs] w
# (ok_open,asmFile,w) = fopen "output.asm" FAppendText w
| not ok_open = abort "failed to open"
# asmFile = fwrites x asmFile
# (ok_close,w) = fclose asmFile w
| not ok_close = abort "failed to close"
= writeToAsm xs w

writeToAsm2:: [String] [String] *f -> [String]| FileSystem f
writeToAsm2 [] acc w = acc
writeToAsm2 [x:xs] acc w
# (ok_open,asmFile,w) = fopen "output.asm" FAppendText w
| not ok_open = abort "failed to open"
# asmFile = fwrites x asmFile
# (ok_close,w) = fclose asmFile w
| not ok_close = abort "failed to close"
| (doesStartsWith x "you") || (doesStartsWith x "are") = writeToAsm2 xs [x:acc] w
| otherwise	= writeToAsm2 xs acc w

doesStartsWith:: String String-> Bool
doesStartsWith str substr
# list = [ x \\ x <-: str]
# sublist = [ x \\ x <-: substr]
| sublist == take (length sublist) list	= True
| otherwise = False

Start w
// 1. Receive name of directory from the user.
# (io,w) = stdio w                                	// open stdio
# io = fwrites "Enter name of directory:\n" io    	// ask for name
# (name,io) = freadline io                        	// read in name
# name = name % (0, size name - 2)                	// remove \n from name
# (ok,w) = fclose io w                            	// close stdio
| not ok = abort "Couldn't close stdio"           	// abort in case of failure

// 2. Get a list of all file names in that directory.
# (dir,w) = getDirectoryContents (RelativePath [PathDown name]) w
# fileList = getNamesOfFilesInDirectory (getEntriesList dir)
# (content,w) = readAllLines (getTheFileField (fopen "myDir\\hello.vm" 0 w))
= writeToAsm2 content [] w
//# = appendNumInEndOfVmFiles (getVmFiles fileList) name w 1















