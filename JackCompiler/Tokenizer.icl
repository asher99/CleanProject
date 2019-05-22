implementation module Tokenizer
import StdEnv
import StdFile
import FileManipulation

/*
*	TokenizeMultipleFiles.
*	foreach file in the files list:
*	1. read the input file to list of lines.
*	2. initial the output file.
*	3. start compilation of the input file, using recursion on the list of the file lines.
*/
TokenizeMultipleFiles:: [String] *f -> (Bool,*f) | FileSystem f
TokenizeMultipleFiles [] w = (True,w)
TokenizeMultipleFiles [x:xs] w
// get the file name without '.jack' postfix
# charlist = [ e \\ e <-: x ]
# len = length charlist
# reslist = take (len-5) charlist
# filename = { e \\ e <- reslist }
// Initial the output file content:
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FWriteText w
| not ok_open = abort("failed to open file")
# outFile = fwrites "" outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
// read all lines of current file to list of strings:
# currentFile = "InputFiles\\" +++ x
# (ok_read_open,inputfile,w) = fopen currentFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// parse the file:
# (ok,w) = Tokenize content filename 1 w
| not ok = abort ("Failed to parse file " +++ x +++ ", execution terminated\n")
= TokenizeMultipleFiles xs w

/*
*	Tokenize:
*	1. cast the line form String to list of Chars.
*	2. and enter state zero of the automaton.
*/
Tokenize:: [String] String Int *f -> (Bool,*f) | FileSystem f
Tokenize [] filename num w = (True,w)
Tokenize [x:xs] filename num w
# xstr = [ ch \\ ch <-: x ]
# (ok,w)= FDAutomaton_state_0 xstr filename num w
| not ok = abort ("failed to tokenize line " +++ x +++ "\n")
= Tokenize xs filename (num+1) w

/*
*	FDAutomaton_state_0:
*	currently support only constantStrings tokens.
*	the automaton enter to this state every time we look for another token.
*	3. if we found digit it means integerConstant token, goto state three.
*	4. if we found symbol it means symbol token, goto state four.
*	5. if we found '\"' it means stringConstant token, goto state five.
*
*/
FDAutomaton_state_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_0 [] filename num w = (True,w)
FDAutomaton_state_0 input filename num w
# ch = input !! 0
# input_ = drop 1 input
| isDigit ch  = FDAutomaton_transit_0_3 input filename num w
| isSymbol ch = FDAutomaton_transit_0_4 input filename num w
| ch == '\"'  = FDAutomaton_transit_0_5 input_ filename num w
| ch == ' '	  = FDAutomaton_state_0 (drop 1 input) filename num w
| ch == '\n'  = (True,w)
| otherwise	  = (False,w)


/*
*	FDAutomaton_transit_0_3
*	when we find digit in state zero we go to state three.
*	along the way - we can print the '<integerConstant>' opening tag.
*/
FDAutomaton_transit_0_3:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_0_3 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<integerConstant> "
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_3 input filename num w

/*
*	FDAutomaton_transit_0_4
*	when we find symbol in state zero we go to state four.
*	along the way - we can print the '<symbol>' opening tag.
*/
FDAutomaton_transit_0_4:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_0_4 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<symbol> "
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_4 input filename num w

/*
*	FDAutomaton_transit_0_5
*	when we find the character '\"' in state zero we go to state five.
*	along the way - we can print the '<stringConstant>' opening tag.
*/
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

/*
*	FDAutomaton_state_3:
*	in this state we keep reading the string till we got another '\"' character.
*	when that happend, goto state six.
*/
FDAutomaton_state_3:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_3 [] filename num w = (True,w)
FDAutomaton_state_3 input filename num w
# ch = input !! 0
| not (isDigit ch) = FDAutomaton_transit_3_0 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwritec ch outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# input_ = drop 1 input
= FDAutomaton_state_3 input_ filename num w


/*
*	FDAutomaton_state_4:
*	in this state we print the symbol to the .xml file.
*	TODO: handle special characters.
*	goto state zero.
*/
FDAutomaton_state_4:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_4 [] filename num w = (True,w)
FDAutomaton_state_4 input filename num w
# ch = input !! 0
| isSpecialSymbol ch = handleSpecialCharacter ch filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwritec ch outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# input_ = drop 1 input
= FDAutomaton_transit_4_0 (drop 1 input) filename num w


/*
*	FDAutomaton_state_5:
*	in this state we keep reading the string till we got another '\"' character.
*	when that happend, goto state six.
*/
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


/*
*	FDAutomaton_transit_3_0
*	when in state three and the next character is no longer a digit, goto state zero.
*	the integer is complete - we can print the '</integerConstant>' closing tag.
*/
FDAutomaton_transit_3_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_3_0 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = " </integerConstant>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 input filename num w

/*
*	FDAutomaton_transit_4_0
*	after printing the symbol in state four, goto state zero.
*   we can print the '</symbol>' closing tag.
*/
FDAutomaton_transit_4_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_4_0 input filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = " </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 input filename num w

/*
*	FDAutomaton_transit_5_6
*	when we find the token '\"' in state five we go to state six.
*	the string is complete - we can print the '</stringConstant>' closing tag.
*/
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

/*
*	FDAutomaton_state_6
*	the stringConstant token is complete, return to state zero.
*/
FDAutomaton_state_6:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_6 [] filename num w = (True,w)
FDAutomaton_state_6 input filename num w
= FDAutomaton_state_0 input filename num w

/*
* isSymbol
* check if a character is an identified symbol in 'jack' language
*/
isSymbol :: Char -> Bool
isSymbol ch 
| ch == '{' = True
| ch == '}' = True
| ch == '(' = True
| ch == ')' = True
| ch == '[' = True
| ch == ']' = True
| ch == '.' = True
| ch == ',' = True
| ch == ';' = True
| ch == '+' = True
| ch == '-' = True
| ch == '*' = True
| ch == '/' = True
| ch == '&' = True
| ch == '|' = True
| ch == '<' = True
| ch == '>' = True
| ch == '=' = True
| ch == '~' = True
| otherwise = False

/*
*	isSpecialSymbol
*	there are four special symbols need special handling.
*/
isSpecialSymbol:: Char -> Bool
isSpecialSymbol ch
| ch == '<' = True
| ch == '>' = True
| ch == '"' = True
| ch == '&' = True
| otherwise = False

/*
*	handleSpecialCharacter
*	handle special character by printing the right expression for each special symbol.
*/
handleSpecialCharacter:: Char String Int *f -> (Bool,*f) | FileSystem f
handleSpecialCharacter ch filename num w
| ch == '&' = printAmp ch filename num w
| ch == '<' = printGt  ch filename num w
| ch == '>' = printLt  ch filename num w
| ch == 'W' = printQut ch filename num w

/*
*	printAmp
*	print the equivalent expression to &
*/
printAmp:: Char String Int *f -> (Bool,*f) | FileSystem f
printAmp ch filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&amp </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)

/*
*	printGt
*	print the equivalent expression to &
*/
printGt :: Char String Int *f -> (Bool,*f) | FileSystem f
printGt ch filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&gt </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)

/*
*	printLt
*	print the equivalent expression to &
*/
printLt :: Char String Int *f -> (Bool,*f) | FileSystem f
printLt ch filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&lt </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)

/*
*	printQut
*	print the equivalent expression to &
*/
printQut:: Char String Int *f -> (Bool,*f) | FileSystem f
printQut ch filename num w
# outFile = "OutputFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&quot </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)




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
















