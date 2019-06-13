implementation module symTable
import StdEnv
import StdFile
import Directory
import FileManipulation

insertRecordClassTable:: String String String *f -> (Bool,*f) | FileSystem f
insertRecordClassTable name type kind w
// open the file for reading, get the counter, close the file:
# cFile = "symtables\\classcounter.txt"
# (okopen,cFile,w) = fopen cFile FReadText w
| not okopen = abort("")
# (counterstr,cFile) = freadline cFile
# counter = toInt counterstr
# (okclose,w) = fclose cFile w
// insert record to the symbol record:
# tFile = "symtables\\classtable.txt"
# (okopen,tFile,w) = fopen tFile FAppendText w
| not okopen = abort("")
# tFile = fwrites (name +++ " " +++ type +++ " " +++ kind +++ " " +++ counterstr +++ "\n" ) tFile 
# (okclose,w) = fclose tFile w
// open the file for writing, set the counter, close the file:
# (ok_inc,w) = incIndex counter "symtables\\classcounter.txt" w
| not ok_inc = abort("failed with the counter++ thing")
// return:
= (True,w)

getIndexClassTable:: String *f -> (Bool,String,*f) | FileSystem f
getIndexClassTable name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\classtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# index = findSymbolIndex name content
// return:
= (True,index,w)

insertRecordMethodTable:: String String String *f -> (Bool,*f) | FileSystem f
insertRecordMethodTable name type kind w
// open the file for reading, get the counter, close the file:
# cFile = "symtables\\methodcounter.txt"
# (okopen,cFile,w) = fopen cFile FReadText w
| not okopen = abort("")
# (counterstr,cFile) = freadline cFile
# counter = toInt counterstr
# (okclose,w) = fclose cFile w
// insert record to the symbol record:
# tFile = "symtables\\methodtable.txt"
# (okopen,tFile,w) = fopen tFile FAppendText w
| not okopen = abort("")
# tFile = fwrites (name +++ " " +++ type +++ " " +++ kind +++ " " +++ counterstr +++ "\n" ) tFile 
# (okclose,w) = fclose tFile w
// open the file for writing, set the counter, close the file:
# (ok_inc,w) = incIndex counter "symtables\\methodcounter.txt" w
| not ok_inc = abort("failed with the counter++ thing")
// return:
= (True,w)

incIndex:: Int String *f -> (Bool,*f) | FileSystem f
incIndex counter cFile w
// write counter++
# counter_ = counter + 1
# (okopen2,cFile,w) = fopen cFile FWriteText w
| not okopen2 = abort("")
# cFile = fwrites (toString counter_) cFile
# (okclose2,w) = fclose cFile w
| not okclose2 = abort("")
= (True,w)

getMethodSymbolIndex:: String *f -> (Bool,String,*f) | FileSystem f
getMethodSymbolIndex name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\methodtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# index = findSymbolIndex name content
// return:
= (True,index,w)

findSymbolIndex:: String [String] -> String
findSymbolIndex name [x]
# record = split x
= record!!3

findSymbolIndex name [x:xs]
# record = split x
| not (record!!0 == name) = findSymbolIndex name xs
= record!!3

getMethodSymbolKind:: String *f -> (Bool,String,*f) | FileSystem f
getMethodSymbolKind name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\methodtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# index = findSymbolKind name content
// return:
= (True,index,w)

findSymbolKind:: String [String] -> String
findSymbolKind name [x]
# record = split x
= record!!2

findSymbolKind name [x:xs]
# record = split x
| (record!!0 == name) = record!!2
= findSymbolKind name xs



getMethodTableCounter:: *f -> (Bool,String,*f) | FileSystem f
getMethodTableCounter w
# cFile = "symtables\\methodcounter.txt"
# (ok_read_open,cFile,w) = fopen cFile FReadText w
# (counter,inputfile) = freadline cFile
# (ok_read_close,w) = fclose inputfile w
= (True,counter,w)

getClassTableCounter:: *f -> (Bool,String,*f) | FileSystem f
getClassTableCounter w
# cFile = "symtables\\classcounter.txt"
# (ok_read_open,cFile,w) = fopen cFile FReadText w
# (counter,inputfile) = freadline cFile
# (ok_read_close,w) = fclose inputfile w
= (True,counter,w) 

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


