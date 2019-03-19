definition module FileOperations
import StdEnv
import StdFile

listOfLinesInFile:: *File -> ([String],*File)

recRead :: *File [String] -> ([String],!*File)
