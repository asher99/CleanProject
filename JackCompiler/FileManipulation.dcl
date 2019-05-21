definition module FileManipulation
import Directory
import StdFile

getEntriesList:: (!DirError, [DirEntry]) -> [DirEntry]

getNamesOfFilesInDirectory :: [DirEntry] -> [String]

getTheFileField:: (Bool,*File,*f) -> (*File,*f)

getVmFiles:: [String] -> [String]

isVm:: String -> Bool

readAllLines :: (!*File,*f) -> ([String],*f)

recRead :: *File [String] -> ([String],!*File)

appendNumInEndOfVmFiles :: [String] String !*f Int -> (Bool,*f) | FileSystem f

listOfLinesInFile:: *File -> ([String],*File)

getJackFiles:: [String] -> [String]

isJack:: String -> Bool

//copyToAsm:: *File !*f -> Bool | FileSystem f

//writeToAsm:: [String] *File *f -> Bool

