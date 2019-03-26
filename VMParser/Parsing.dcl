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

parsePushStatic:: String *f -> (Bool,*f) | FileSystem f  

parsePushArgument:: String *f -> (Bool,*f) | FileSystem f  
 
parsePushLocal :: String *f -> (Bool,*f) | FileSystem f  

parsePushThis :: String *f -> (Bool,*f) | FileSystem f  

parsePushThat :: String *f -> (Bool,*f) | FileSystem f  

parsePushTemp :: String *f -> (Bool,*f) | FileSystem f  

parsePopStatic:: String *f -> (Bool,*f) | FileSystem f  

parsePopArgument:: String *f -> (Bool,*f) | FileSystem f  

parsePopLocal :: String *f -> (Bool,*f) | FileSystem f  

parsePopThis :: String *f -> (Bool,*f) | FileSystem f  

parsePopThat :: String *f -> (Bool,*f) | FileSystem f  

parsePopTemp :: String *f -> (Bool,*f) | FileSystem f  

parsePushPointer0:: String *f -> (Bool,*f) | FileSystem f  

parsePushPointer1:: String *f -> (Bool,*f) | FileSystem f  

parsePopPointer0:: String *f -> (Bool,*f) | FileSystem f  

parsePopPointer1:: String *f -> (Bool,*f) | FileSystem f  











