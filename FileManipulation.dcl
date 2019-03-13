definition module FileManipulation
import Directory
import StdFile

getEntriesList:: (!DirError, [DirEntry]) -> [DirEntry]

getNamesOfFilesInDirectory :: [DirEntry] -> [String]

getTheFileField:: (Bool,*File,*f) -> *File

getVmFiles:: [String] -> [String]

isVm:: String -> Bool
