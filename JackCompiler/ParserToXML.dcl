definition module ParserToXML
import StdEnv
import StdFile

ParseMultipleFiles:: [String] *f -> (Bool,*f) | FileSystem f

startParsing ::  [String] String Int *f -> (Bool,*f) | FileSystem f
