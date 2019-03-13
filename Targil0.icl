/*
Targil 0 using Clean programming language,
Programming Languages Principles course 2019.

By Asher Alexander		206195356	
&& Zvei Eliezer Nir		316525344

*/

module Targil0

import StdEnv
import FileManipulation



Start w

// 1. Receive name of directory from the user.
# (io,w) = stdio w                                // open stdio
# io = fwrites "Enter name of directory:\n" io    // ask for name
# (name,io) = freadline io                        // read in name
# name = name % (0, size name - 2)                // remove \n from name
//# io = fwrites ("Hello, " +++ name +++ "!\n") io  // greet user
# (ok,w) = fclose io w                            // close stdio
| not ok = abort "Couldn't close stdio"           // abort in case of failure

// 2. Get a list of all file names in that directory.
# (dir,w) = getDirectoryContents (RelativePath [PathDown name]) w
# fileList = getNamesOfFilesInDirectory (getEntriesList dir)

// 3. return a sublist of all '.vm' files.
= appendNumToAllFiles(getVmFiles fileList)

appendNumToAllFiles :: [String] String *f -> Bool
appendNumToAllFiles [] dirName q = True
appendNumToAllFiles [hd:tl] dirName q = appendNumToAllFiles tl dirName q
//	# fwrites("947" dirName +++ "\\\\" +++ hd)	



















