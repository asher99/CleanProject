definition module Parsing
import StdEnv
import StdFile

parse:: [String] Int *f-> (Bool,*f) | FileSystem f

parseLine:: String Int *f-> (Bool,*f) | FileSystem f

parsePushConstant:: String *f -> (Bool,*f) | FileSystem f  

parseAddCommand:: String *f -> (Bool,*f) | FileSystem f  

parseSubCommand:: String *f -> (Bool,*f) | FileSystem f  

parseNegCommand:: String *f -> (Bool,*f) | FileSystem f  

parseAndCommand:: String *f -> (Bool,*f) | FileSystem f  

parseOrCommand:: String *f -> (Bool,*f) | FileSystem f  

parseNotCommand:: String *f -> (Bool,*f) | FileSystem f 

parseEQCommand:: String Int *f -> (Bool,*f) | FileSystem f  

parseGTCommand:: String Int *f -> (Bool,*f) | FileSystem f  

parseLTCommand:: String Int *f -> (Bool,*f) | FileSystem f  


 

