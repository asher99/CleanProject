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
# counter_ = counter + 1
# cFile = "symtables\\classcounter.txt"
# (okopen,cFile,w) = fopen cFile FWriteText w
| not okopen = abort("")
# cFile = fwrites (toString counter_) cFile
# (okclose,w) = fclose cFile w
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
# index = findRecordIndex name content
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
# counter_ = counter + 1
# cFile = "symtables\\classcounter.txt"
# (okopen,cFile,w) = fopen cFile FWriteText w
| not okopen = abort("")
# cFile = fwrites (toString counter_) cFile
# (okclose,w) = fclose cFile w
// return:
= (True,w)

getIndexMethodTable:: String *f -> (Bool,String,*f) | FileSystem f
getIndexMethodTable name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\methodtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# index = findRecordIndex name content
// return:
= (True,index,w)

findRecordIndex:: String [String] -> String
findRecordIndex name [x:xs]
# record = split x
| not (record!!0 == name) = findRecordIndex name xs
= record!!3


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


