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
# file = fwrites "@0\t//Initial SP\nM=A\n\n" file
# (ok_close,w) = fclose file w
| not ok_close = abort "failed to close output file.\n"

// 4. Parse the content of the file.
= parse content w



