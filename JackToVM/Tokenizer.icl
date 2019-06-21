implementation module Tokenizer
import StdEnv
import StdFile
import FileManipulation
import ClearComments
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
// read all lines of current file to list of strings:
# currentFile = "jackFiles\\" +++ x
# (ok_read_open,inputfile,w) = fopen currentFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w
// Initial the No-Comment file content:
# outFile = "NoCommentFiles\\" +++ x
# (ok_open,outFile,w) = fopen outFile FWriteText w
| not ok_open = abort("failed to open file")
# outFile = fwrites "" outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
// remove all comments
# (comments_removed,w) = clearComments content filename w
| not comments_removed = abort("problem with the comments")
// read all lines of No-Comment v.ersion of the file to list of strings:
# currentFile_NC = "NoCommentFiles\\" +++ x
# (ok_read_NC,inputfile_NC,w) = fopen currentFile_NC FReadText w
# (content_NC,inputfile_NC) = listOfLinesInFile inputfile_NC
# (ok_read_NC_close,w) = fclose inputfile_NC w
// Initial the output file content:
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FWriteText w
| not ok_open = abort("failed to open file")
# outFile = fwrites "<tokens>\n" outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
// Tokenize the file:
# (ok,w) = Tokenize content_NC filename 1 w
| not ok = abort ("Failed to parse file " +++ x +++ ", execution terminated\n")
# (tokenized,w) = TokenizeMultipleFiles xs w
| not tokenized = abort("failed to tokenize file " +++ filename +++ ".jack\n")
// Finish the output file content:
# outFile2 = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# outFile2 = fwrites "</tokens>\n" outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
| ok_read_close2 = (True,w)


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
*	1. if we found alpha we still don't know if its identifier or key word, so we goto state one to determine.
*	3. if we found digit it means integerConstant token, goto state three.
*	4. if we found symbol it means symbol token, goto state four.
*	5. if we found '\"' it means stringConstant token, goto state five.
*	6. if we found '//' we can ignore the line.
*	7. if we found  just '/' it maybe followd by '*' and than we need to read till we find another  
*/
FDAutomaton_state_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_0 [] filename num w = (True,w)
FDAutomaton_state_0 input filename num w
# ch = input !! 0
# ch2 = input !! 1
# input_ = drop 1 input
| (ch == '/') && (ch2 == '/') = (True,w)
| (ch == '_') || (isAlpha ch)	= FDAutomaton_transit_0_1 input filename num w
| isDigit ch  	= FDAutomaton_transit_0_3 input filename num w
| isSymbol ch 	= FDAutomaton_transit_0_4 input filename num w
| ch == '\"'  	= FDAutomaton_transit_0_5 input_ filename num w
| isSpace ch  	= FDAutomaton_state_0 (drop 1 input) filename num w
| ch == '\n'  	= (True,w)
| otherwise	  	= (False,w)


/*
*	FDAutomaton_transit_0_1
*	we initial buffer for state one, where we read the whole word to the buffer before we compare it to the keywords.
*/
FDAutomaton_transit_0_1:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_0_1 input filename num w
= FDAutomaton_state_1 input [] filename num w

/*
*	FDAutomaton_transit_0_3
*	when we find digit in state zero we go to state three.
*	along the way - we can print the '<integerConstant>' opening tag.
*/
FDAutomaton_transit_0_3:: [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_transit_0_3 input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<stringConstant> "
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_5 input filename num w

/*
*	FDAutomaton_state_1
*	keep reading the input word to buffer till we find a white-space or a symbol
*/
FDAutomaton_state_1:: [Char] [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_1 input buff filename num w
# ch = input !! 0
| (isSpace ch) || (isSymbol ch) = FDAutomaton_state_11 input buff filename num w
# chlist = [ch]
# buff_ = buff ++ chlist
= FDAutomaton_state_1 (drop 1 input) buff_ filename num w

/*
*	FDAutomaton_state_11
*	compare the word in buffer to the stored keywords.
*	goto substate 12 if its keyword, else goto 13 .
*/
FDAutomaton_state_11:: [Char] [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_11 input buff filename num w
| isKeyword (toString buff) = FDAutomaton_state_12 input buff filename num w
= FDAutomaton_state_13 input buff filename num w

/*
*	FDAutomaton_state_12
*	print a whole 'keyword' token.
*/
FDAutomaton_state_12:: [Char] [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_12 input buff filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<keyword> " +++ { ch \\ ch <- buff } +++ " </keyword>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 input filename num w

/*
*	FDAutomaton_state_13
*	print a whole 'identifier' token.
*/
FDAutomaton_state_13:: [Char] [Char] String Int *f -> (Bool,*f) | FileSystem f
FDAutomaton_state_13 input buff filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<identifier> " +++ { ch \\ ch <- buff } +++ " </identifier>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 input filename num w

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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
| isSpecialSymbol ch = handleSpecialCharacter ch input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
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
handleSpecialCharacter:: Char [Char] String Int *f -> (Bool,*f) | FileSystem f
handleSpecialCharacter ch input filename num w
| ch == '&' = printAmp ch input filename num w
| ch == '<' = printLt  ch input filename num w
| ch == '>' = printGt  ch input filename num w
| ch == 'W' = printQut ch input filename num w

/*
*	printAmp
*	print the equivalent expression to &
*/
printAmp:: Char [Char] String Int *f -> (Bool,*f) | FileSystem f
printAmp ch input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&amp; </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 (drop 1 input) filename num w

/*
*	printGt
*	print the equivalent expression to &
*/
printGt :: Char [Char] String Int *f -> (Bool,*f) | FileSystem f
printGt ch input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&gt; </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 (drop 1 input) filename num w

/*
*	printLt
*	print the equivalent expression to &
*/
printLt :: Char [Char] String Int *f -> (Bool,*f) | FileSystem f
printLt ch input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&lt; </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 (drop 1 input) filename num w

/*
*	printQut
*	print the equivalent expression to &
*/
printQut:: Char [Char] String Int *f -> (Bool,*f) | FileSystem f
printQut ch input filename num w
# outFile = "TxmlFiles\\" +++ filename +++ "T.xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "&quot; </symbol>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= FDAutomaton_state_0 (drop 1 input) filename num w



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

/*
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
















