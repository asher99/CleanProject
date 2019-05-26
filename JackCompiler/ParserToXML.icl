implementation module ParserToXML
import StdEnv
import StdFile
import Directory
import FileManipulation

/*
*	ParseMultipleFiles.
*	foreach file in the files list:
*	1. read the input file to list of lines.
*	2. initial the output file.
*	3. start compilation of the input file, using recursion on the list of the file lines.
*/
ParseMultipleFiles:: [String] *f -> (Bool,*f) | FileSystem f
ParseMultipleFiles [] w = (True,w)
ParseMultipleFiles [x:xs] w
// get the file name without 'T.xml' postfix
# charlist = [ e \\ e <-: x ]
# len = length charlist
# reslist = take (len-5) charlist
# filename = { e \\ e <- reslist }
// Initial the output file content:
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FWriteText w
| not ok_open = abort("failed to open file")
# outFile = fwrites "" outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
// read all lines of current file to list of strings:
# currentFile = "TxmlFiles\\" +++ x 
# (ok_read_open,inputfile,w) = fopen currentFile FReadText w
# (content,inputfile) = listOfLinesInFile inputfile
# (ok_read_close,w) = fclose inputfile w

// parse the file:
# (ok,w) = startParsing content filename 1 w
| not ok = abort ("Failed to parse file " +++ x +++ ", execution terminated\n")
# (parsed,w) = ParseMultipleFiles xs w
| not parsed = abort("failed to parse file " +++ filename +++ "T.xml\n")



/*
*	parse:
*	1. cast the line form String to list of Chars.
*	2. and start looking for the first rule.
*/
startParsing ::  [String] String Int *f -> (Bool,*f) | FileSystem f
startParsing [] filename num w = (True,w)
startParsing [x:xs] filename num w
# xstr = toString [ ch \\ ch <-: x ]
| xstr == "<tokens>\n" = startParsing xs filename num w
| xstr == "<keyword> class </keyword>\n" = parseClass xs filename num w
| otherwise = abort("This is why you fail")//(False,w)






/************* PROGRAM STRUCTURE ***************/

parseClass :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseClass [class_name,sym:xs] filename num w 
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print_start = "<class>\n" +++ "<keyword> class </keyword>" +++ "\n" +++ class_name +++ sym 
# outFile = fwrites string_to_print_start outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
//#(ok_parse_classVarDec,xs,w) = parseClassVarDec xs filename num w
//| not ok_parse_classVarDec = abort("failed to parse subroutine")
#(ok_parse_subroutine,w) = parseSubroutineDec xs filename num w
| not ok_parse_subroutine = abort("failed to parse subroutine")


# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "<symbol> } </symbol>\n</class>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,w)

parseSubroutineDec :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseSubroutineDec [subroutine_kw, subroutine_type ,subroutine_name, sym:xs] filename num w
| not ((subroutine_kw == "<keyword> function </keyword>\n") || (subroutine_kw == "<keyword> function </keyword>\n") || (subroutine_kw == "<keyword> function </keyword>\n")) = (True,w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<subroutineDec>\n" +++ subroutine_kw +++ subroutine_type +++ subroutine_name +++ sym
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= parseSubroutineDec xs filename num w


parseClassVarDec :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseClassVarDec [classVarDec_kw,classVarDec_type:xs] filename num w
| not ((classVarDec_kw == "<keyword> static </keyword>\n") || (classVarDec_kw == "<keyword> field </keyword>\n")  = (True,w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<classVarDec>\n" +++ classVarDec_kw +++ classVarDec_type

# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# (ok_parsed_all_sym,xs,w) = parseAllVars xs filename num w // calls a method that will write to file all the : type varName, type varName ... 

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</classVarDec>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,xs,w)

/* parses all the symicollons in ClassVarDec */
parseAllVars :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseAllVars [varName,sym:xs] filename num w

# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = varName +++ sym
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
| sym == "<symbol> , </symbol>\n" = parseAllVars xs filename num w // until we get a symbol of ;
= (True,xs,w)

parseType :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseType [x:xs] filename num w
| not ((x == "<keyword> int </keyword>\n") || (x == "<keyword> int </keyword>\n") || (x == "<keyword> int </keyword>\n") || (x == "<keyword> int </keyword>\n")) = parseClassName [x,xs] filename num w
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = x
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file") 
= (True,w)

/*
parseParameterList ::

parseSubroutineBody ::

parseVarDec :: 

/* terminal of identifier */
parseClassName :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseClassName [x:xs] filename num w
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = x
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file") 
= (True,w)



/* terminal of identifier */
parseSubroutineName ::

/* terminal of identifier */
parseVarName :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseVarName [x:xs] filename num w
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = x
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)


/************* STATEMENTS ***************/

parseStatements ::

parseStatement ::

parseLetStatement ::

parseIfStatement ::

parseWhileStatment ::

parseDoStatment ::

parseReturnStatement ::

/************* EXPRESSIONS ***************/

parseExpression ::

parseTerm ::

parseSubroutineCall ::

parseExpressionList ::

/* terminal of symbol */

parseOp ::

/* terminal of symbol */

parseUnaryOp ::

/* terminal of keyword */

parseKeywordConstant ::


/************* GETTERS ***************/
getKeyword:: String -> String


getIdentifier:: 

getSymbol:: 

*/

























