module JackCompiler
import StdEnv
import StdFile
import Directory
import FileManipulation
import Tokenizer
import ParserToXML

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

Start w
// 1. Welcome
# (io,w) = stdio w                                				// open stdio
# io = fwrites "JACK COMPILER by Asher Alexander & Zvei Eliezer Nir\n" io


// Tokenizer:

// 2. Import the content of InputFiles directory
# (dir,w) = getDirectoryContents (RelativePath [PathDown "jackFiles"]) w
# filesList = getJackFiles (getNamesOfFilesInDirectory (getEntriesList dir))
# io = fwrites "The compiler found the following .VM files: " io
# io = printList filesList io
# (ok_token,w) =  TokenizeMultipleFiles filesList w
| not ok_token = abort("failed to tokenize")
 
// Parser:
# (dir,w) = getDirectoryContents (RelativePath [PathDown "TxmlFiles"]) w 
# filesList = getTxmlFiles (getNamesOfFilesInDirectory (getEntriesList dir))
# io = fwrites "The compiler found the following .Txml files: " io
# io = printList filesList io
= ParseMultipleFiles filesList w












