implementation module FileOperations
import StdEnv
import StdFile

/*
* Resturn list of Strings - lines in a file.
*/
listOfLinesInFile:: *File -> ([String],*File)
listOfLinesInFile file
# (list,file) = recRead file []
= (reverse list, file)

/*
* Recursive reading of a file along with storing each line as a String in a list of Strings.
*/
recRead :: *File [String] -> ([String],!*File)
recRead file acc
	# (string, file) = freadline file
	# (err,file)	 = ferror file
	| err			 = (["Error Happend"],file)
	| string == ""   = (acc, file)
	| otherwise      = recRead file [string:acc]