implementation module Tokenizer
import StdEnv
import StdFile
import FileManipulation

/*
*	Split
*/
split :: String -> [String]
split s = scrape (fromString s)
where
    scrape :: [Char] -> [String]
    scrape [] = []
    scrape cs=:[c:_]
    | isSpace c = scrape (dropWhile isSpace cs)
    | otherwise = [toString word:scrape rest]
    where
        (word,rest) = span (not o isSpace) cs



TokenizeMultipleFiles:: [String] *f -> (Bool,*f) | FileSystem f
TokenizeMultipleFiles [] w = (True,w)
TokenizeMultipleFiles [x:xs] w
// read all lines of current file to list of strings:
# currentFile = "InputFiles\\" +++ x
# (ok_read_open,inputfile,w) = fopen currentFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// parse the file:
# (ok,w) = Tokenize content x 1 w
| not ok = abort ("Failed to parse file " +++ x +++ ", execution terminated\n")
= TokenizeMultipleFiles xs w

Tokenize:: [String] String Int *f -> (Bool,*f) | FileSystem f
Tokenize [] filename num w = (True,w)
Tokenize [x:xs] filename num w
# xstr = [ ch \\ ch <-: x ]
# (ok,w)= FDAutomaton_state_0 xstr filename num w
| not ok = abort ("failed to tokenize line " +++ x +++ "\n")
= Tokenize xs filename (num+1) w


FDAutomaton_state_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_0 [] filename num w = (True,w)
FDAutomaton_state_0 input filename num w
# ch = input !! 0
# input_ = drop 1 input
| ch == '\"' = FDAutomaton_transit_0_5 input_ filename num w
| ch == ' ' = FDAutomaton_state_0 (drop 1 input) filename num w

| otherwise = (False,w)

FDAutomaton_transit_0_5:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_0_5 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<stringConstant> "
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_5 input filename num w

FDAutomaton_state_5:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_5 [] filename num w = (True,w)
FDAutomaton_state_5 input filename num w
# ch = input !! 0
| ch == '\"' = FDAutomaton_transit_5_6 (drop 1 input) filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwritec ch outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# input_ = drop 1 input
= FDAutomaton_state_5 input_ filename num w

FDAutomaton_transit_5_6:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_5_6 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = " </stringConstant>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_6 input filename num w

FDAutomaton_state_6:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_6 [] filename num w = (True,w)
FDAutomaton_state_6 input filename num w
= FDAutomaton_state_0 input filename num w

/*
TokenizeLine:: [String] String Int *f-> (Bool,*f) | FileSystem f 
TokenizeLine [] filename num w = (True,w)
TokenizeLine [x:xs] filename num w
# (ok,w)= FDAutomaton x filename num w
| not ok = abort ("failed to tokenize word: " +++ x +++ "\n")
= TokenizeLine xs filename num w

FDAutomaton:: String String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton word filename num w
# ok_kw = isKeyword word
| ok_kw = printKeywordTag word filename num w
# ok_sym = isSymbol word
| ok_sym = printSymbolTag word filename num w
= (True,w)

isKeyword:: String -> Bool
isKeyword word
| word == "class" = True
| word == "constructor" = True
| word == "function" = True
| word == "method" = True
| word == "field" = True
| word == "static" = True
| word == "var" = True
| word == "int" = True
| word == "char" = True
| word == "boolean" = True
| word == "void" = True
| word == "true" = True
| word == "false" = True
| word == "null" = True
| word == "this" = True
| word == "let" = True
| word == "do" = True
| word == "if" = True
| word == "else" = True
| word == "while" = True
| word == "return" = True
| otherwise = False

printKeywordTag:: String String Int *f -> (Bool,*f) | FileSystem f
printKeywordTag word filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,inputfile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<keyword> " +++ word +++ " </keyword>\n"
# inputfile = fwrites string_to_print inputfile
# (ok_read_close,w) = fclose inputfile w
| not ok_read_close = abort("failed to close file")
= (True,w)

isSymbol:: String -> Bool
isSymbol word
| word = "" = true
= False

printSymbolTag:: String String Int *f -> (Bool,*f) | FileSystem f
printSymbolTag word filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,inputfile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<symbol> " +++ word +++ " </symbol>\n"
# inputfile = fwrites string_to_print inputfile
# (ok_read_close,w) = fclose inputfile w
| not ok_read_close = abort("failed to close file")
= (True,w)
*/
















