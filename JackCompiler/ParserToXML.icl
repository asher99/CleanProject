implementation module ParserToXML
import StdEnv
import StdFile
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
# outFile = "OutputFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FWriteText w
| not ok_open = abort("failed to open file")

// read all lines of current file to list of strings:
# currentFile = "InputFiles\\" +++ x
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
# xstr = [ ch \\ ch <-: x ]
| xstr == "<keyword> class </keyword>" = parseClass xs filename num w
|
| otherwise = 






/************* PROGRAM STRUCTURE ***************/

parseClass :: [String] String Int *f -> (Bool,*f) | FileSystem f
parseClass input filename num w 
# outFile = "OutputFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<class> \n" +++ input
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")


parseClassVarDec ::

parseType :: 

parseSubroutineDec ::

parseParameterList ::

parseSubroutineBody ::

parseVarDec :: 

/* terminal of identifier */
parseClassName :: 

/* terminal of identifier */
parseSubroutineName ::

/* terminal of identifier */
parseVarName ::

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





























