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
getTheFileField:: (Bool,*File,*f) -> *File
getTheFileField (x,y,z)
	| x == True		= y
	| otherwise		= abort("Failed to open the file...")

getVmFiles:: [String] -> [String]
getVmFiles listOfFiles = filter isVm listOfFiles

isVm:: String -> Bool
isVm fileName
	| take 3 (reverse nameAsList) == vm 	= True
	| otherwise								= False
where
	nameAsList = [ e \\ e <-: fileName ]
	vm = ['m', 'v', '.']
	
	
/*
first (x,y) = x

readFile:: *File -> [{#Char}]
readFile file 
	| first (fend (file))	= []
	| otherwise				= [ line : readFile (file)] 
where
	line = first(freadline (file))
*/