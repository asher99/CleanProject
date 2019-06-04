implementation module FileManipulation

import StdEnv
import StdFile
import Directory
import FileManipulation

// The getDirectoryContents return a tuple: (Status of the directory, [list of the directory entries])
// Since we want to get the names of all the files in the directory - lets extract the list.
getEntriesList:: (!DirError, [DirEntry]) -> [DirEntry]
getEntriesList (x,y) = y

// Now that we have list of DirEntry Records, its easy to return another list with the names of those entries.
getNamesOfFilesInDirectory :: [DirEntry] -> [String]
getNamesOfFilesInDirectory [x] = [x.fileName]
getNamesOfFilesInDirectory [x:xs] = [x.fileName : getNamesOfFilesInDirectory xs]

// Receive the result of the 'fopen' function and return the File handle in case 'fopen' succeed.
getTheFileField:: (Bool,*File,*f) -> (*File,*f)
getTheFileField (x,y,z)
	| x == True		= (y,z)
	| otherwise		= abort("Failed to open the file...")

getVmFiles:: [String] -> [String]
getVmFiles listOfFiles = filter isVm listOfFiles

getJackFiles:: [String] -> [String]
getJackFiles listOfFiles = filter isJack listOfFiles

getTxmlFiles:: [String] -> [String]
getTxmlFiles listOfFiles = filter isTxml listOfFiles

isVm:: String -> Bool
isVm fileName
	| take 3 (reverse nameAsList) == vm 	= True
	| otherwise								= False
where
	nameAsList = [ e \\ e <-: fileName ]
	vm = ['m', 'v', '.']

isJack:: String -> Bool
isJack fileName
	| take 5 (reverse nameAsList) == jack 	= True
	| otherwise								= False
where
	nameAsList = [ e \\ e <-: fileName ]
	jack = ['k','c','a', 'j', '.']


isTxml:: String -> Bool
isTxml fileName
	| take 5 (reverse nameAsList) == jack 	= True
	| otherwise								= False
where
	nameAsList = [ e \\ e <-: fileName ]
	jack = ['l','m', 'x', '.','T']


// read file content to list of Stringd, each String is a line.	
readAllLines :: (!*File,*f) -> ([String],*f)
readAllLines (file,w)
# (result,file) = recRead file []
= (reverse result,w)
// helping recursion method
recRead :: *File [String] -> ([String],!*File)
recRead file acc
	# (string, file) = freadline file
	# (err,file)	 = ferror file
	| err			 = (["Error Happend"],file)
	| string == ""   = (acc, file)
	| otherwise      = recRead file [string:acc]
	
	
	
appendNumInEndOfVmFiles :: [String] String !*f Int -> (Bool,*f) | FileSystem f
appendNumInEndOfVmFiles [] dirname w num = (True,w)
appendNumInEndOfVmFiles [x:xs] dirname w num
# path = dirname +++ "\\\\" +++ x
# (ok,file,w) = fopen path FAppendText w
| not ok = abort ("failed to open" +++ path)
# file = fwritei num file 
# (ok2,w) = fclose file w
| not ok2 = abort "failed"
= appendNumInEndOfVmFiles xs dirname w (inc num)

/*
* Resturn list of Strings - lines in a file.
*/
listOfLinesInFile:: *File -> ([String],*File)
listOfLinesInFile file
# (list,file) = recRead file []
= (reverse list, file)


/*copyToAsm:: *File !*f -> Bool | FileSystem f
copyToAsm vmFile w
# lines = readAllLines vmFile
# (ok,asmFile,w) = fopen "output.asm" FAppendText w
| not ok = abort "something wrong"
= writeToAsm lines asmFile w*/

/*writeToAsm:: [String] *File *f -> Bool
writeToAsm [] asmFile w = True
writeToAsm [x:xs] asmFile w
# (ok_open,asmFile,w) = fopen "output.asm" FAppendText w
| not ok_open = abort "failed to open"
# asmFile = fwrites x asmFile
# (ok_close,w) = fclose asmFile w
| not ok_close = abort "failed to close"
= writeToAsm xs asmFile w*/
