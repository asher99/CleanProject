implementation module ClearComments
import StdEnv
import StdFile

clearComments::[String] String  *f -> (Bool,*f) | FileSystem f
clearComments input filename w
# chars = readAllCharactersOfFile input []
# (finished,w) = writeCharactersToFile chars False filename w
| finished = (True,w)


readAllCharactersOfFile:: [String] [Char] -> [Char]
readAllCharactersOfFile [] acc = acc
readAllCharactersOfFile [x:xs] acc
# charlist = [ e \\ e <-: x ]
# acc_ = acc ++ charlist
= readAllCharactersOfFile xs acc_

/**
*	writeCharactersToFile:
*	scan the file for comments.
*	this method had a boolean flag to note if its currently reading a comment (True) or text (False). 
*/
writeCharactersToFile:: [Char] Bool String *f -> (Bool,*f) | FileSystem f
writeCharactersToFile [] False filename  w = (True,w)

writeCharactersToFile [ch] False filename w
# outFile = "NoCommentFiles\\" +++ filename +++ ".jack"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwritec ch outFile
# (ok_write_close,w) = fclose outFile w
| not ok_write_close = abort("failed to close file")
= writeCharactersToFile [] False filename w

writeCharactersToFile [ch1,ch2:chars] False filename w
// look for comments:
| (ch1 == '/') && (ch2 == '*') = writeCharactersToFile chars True filename w
// otherwise: write char
# outFile = "NoCommentFiles\\" +++ filename +++ ".jack"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwritec ch1 outFile
# (ok_write_close,w) = fclose outFile w
| not ok_write_close = abort("failed to close file")
= writeCharactersToFile [ch2:chars] False filename w

writeCharactersToFile [ch1,ch2:chars] False filename w
// look for comment end:
| (ch1 == '*') && (ch2 == '/') = writeCharactersToFile chars False filename w
// otherwise: continue to read.
= writeCharactersToFile [ch2:chars] True filename w



