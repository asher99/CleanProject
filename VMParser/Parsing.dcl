definition module Parsing
import StdEnv
import StdFile

parse:: [String] *f-> (Bool,*f) | FileSystem f

parseLine:: String *f-> (Bool,*f) | FileSystem f

parsePushConstant:: String *f -> (Bool,*f) | FileSystem f  

parseAddCommand:: String *f -> (Bool,*f) | FileSystem f  

