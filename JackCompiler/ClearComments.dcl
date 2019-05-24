definition module ClearComments
import StdEnv
import StdFile

clearComments::[String] String  *f -> (Bool,*f) | FileSystem f

readAllCharactersOfFile:: [String] [Char] -> [Char]

writeCharactersToFile:: [Char] Bool String *f -> (Bool,*f) | FileSystem f
