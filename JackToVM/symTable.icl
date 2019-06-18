implementation module symTable
import StdEnv
import StdFile
import Directory
import FileManipulation

insertRecordClassTable:: String String String *f -> (Bool,*f) | FileSystem f
insertRecordClassTable name type kind w //= abort(kind)
| kind == "static " = insertStatic name type kind w
| kind == "field "  = insertField name type kind w


insertStatic:: String String String *f -> (Bool,*f) | FileSystem f
insertStatic name type kind w
// open the file for reading, get the counter, close the file:
# cFile = "symtables\\classstaticcounter.txt"
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
# (ok_inc,w) = incIndex counter "symtables\\classstaticcounter.txt" w
| not ok_inc = abort("failed with the counter++ thing")
// return:
= (True,w)

insertField:: String String String *f -> (Bool,*f) | FileSystem f
insertField name type kind w
// open the file for reading, get the counter, close the file:
# cFile = "symtables\\classfieldcounter.txt"
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
# (ok_inc,w) = incIndex counter "symtables\\classfieldcounter.txt" w
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


getClassSymbolIndex:: String *f -> (Bool,String,*f) | FileSystem f
getClassSymbolIndex name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\classtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# index = findSymbolIndex name content
// return:
| index == "false" = (False,"failed",w)
= (True,index,w)

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
| index == "false" = (False,"failed",w)
= (True,index,w)

findSymbolIndex:: String [String] -> String
findSymbolIndex name [] = "false"

findSymbolIndex name [x] //= abort(name)
# record = split x
| not ((record!!0 +++ " ")== name) = findSymbolIndex name []
= record!!3

findSymbolIndex name [x:xs]
# record = split x
| not ((record!!0 +++ " ")== name) = findSymbolIndex name xs
= record!!3

getClassSymbolKind:: String *f -> (Bool,String,*f) | FileSystem f
getClassSymbolKind name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\classtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# kind = findSymbolKind name content
// return:
| kind == "field" = (True,"this",w)
= (True,kind,w)

getMethodSymbolKind:: String *f -> (Bool,String,*f) | FileSystem f
getMethodSymbolKind name w
// open the file for reading, get the content as list of lines
# tFile = "symtables\\methodtable.txt"
# (ok_read_open,inputfile,w) = fopen tFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// get the index based on the record name
# kind = findSymbolKind name content
// return:
= (True,kind,w)

findSymbolKind:: String [String] -> String
findSymbolKind name [x]
# record = split x
= record!!2

findSymbolKind name [x:xs]
# record = split x
| ((record!!0 +++ " ")== name) = record!!2
= findSymbolKind name xs



getMethodTableCounter:: *f -> (Bool,String,*f) | FileSystem f
getMethodTableCounter w
# cFile = "symtables\\methodcounter.txt"
# (ok_read_open,cFile,w) = fopen cFile FReadText w
# (counter,inputfile) = freadline cFile
# (ok_read_close,w) = fclose inputfile w
= (True,counter,w)

getClassStaticTableCounter:: *f -> (Bool,String,*f) | FileSystem f
getClassStaticTableCounter w
# cFile = "symtables\\classstaticcounter.txt"
# (ok_read_open,cFile,w) = fopen cFile FReadText w
# (counter,inputfile) = freadline cFile
# (ok_read_close,w) = fclose inputfile w
= (True,counter,w) 

getClassFieldTableCounter:: *f -> (Bool,String,*f) | FileSystem f
getClassFieldTableCounter w
# cFile = "symtables\\classfieldcounter.txt"
# (ok_read_open,cFile,w) = fopen cFile FReadText w
# (counter,inputfile) = freadline cFile
# (ok_read_close,w) = fclose inputfile w
= (True,counter,w) 

fetchVariableFromTables:: String *f -> (Bool,String,String,*f) | FileSystem f
fetchVariableFromTables name w
# (inMethod,index,w) = getMethodSymbolIndex name w
| inMethod = getMethodVariableParams name index w
# (inClass,index,w) = getClassSymbolIndex name w
| not inClass = (False,"","",w)
= getClassVariableParams name index w

getClassVariableParams:: String String *f -> (Bool,String,String,*f) | FileSystem f
getClassVariableParams name index w 
# (ok_kind,kind,w) = getClassSymbolKind name w
= (True,index,kind,w)

getMethodVariableParams:: String String *f -> (Bool,String,String,*f) | FileSystem f
getMethodVariableParams name index w 
# (ok_kind,kind,w) = getMethodSymbolKind name w
= (True,index,kind,w)

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


