definition module Parsing
import StdEnv
import StdFile

parse:: [String] *f-> (Bool,*f) | FileSystem f

parseLine:: String *f-> (Bool,*f) | FileSystem f

parsePushConstant:: String *f -> (Bool,*f) | FileSystem f  

parseAddCommand:: String *f -> (Bool,*f) | FileSystem f  

parseSubCommand:: String *f -> (Bool,*f) | FileSystem f  

parseNegCommand:: String *f -> (Bool,*f) | FileSystem f  

parseAndCommand:: String *f -> (Bool,*f) | FileSystem f  

parseOrCommand:: String *f -> (Bool,*f) | FileSystem f  
