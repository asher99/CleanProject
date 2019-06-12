implementation module ParserToVM
import StdEnv
import StdFile
import Directory
import FileManipulation
import Tokenizer

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
#(ok_parse_classVarDec,xs_,w) = parseClassVarDec xs filename num w
| not ok_parse_classVarDec = abort("failed to parse vardec")
#(ok_parse_subroutine,xs__,w) = parseSubroutineDec xs_ filename num w
| not ok_parse_subroutine = abort("failed to parse subroutine")


# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "<symbol> } </symbol>\n</class>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,w)

parseSubroutineDec :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineDec [subroutine_kw, subroutine_type ,subroutine_name, sym:xs] filename num w
| not ((subroutine_kw == "<keyword> function </keyword>\n") || (subroutine_kw == "<keyword> method </keyword>\n") || (subroutine_kw == "<keyword> constructor </keyword>\n")) = (True,[subroutine_kw, subroutine_type ,subroutine_name, sym:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<subroutineDec>\n" +++ subroutine_kw +++ subroutine_type +++ subroutine_name +++ sym +++ "<parameterList>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_params,[sym_:xs_],w) = parseParameterList xs filename num w
| not ok_params = abort("failed to parse params list")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</parameterList>\n" +++ sym_
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")

# (ok_body,xs__,w) = parseSubroutineBody xs_ filename num w
# (okw,w) = write2file "</subroutineDec>\n" filename w
//| ok_body = abort(xs__!!0+++xs__!!1+++xs__!!2)
| not ok_body  = abort("failed to parse subroutine body")
= parseSubroutineDec xs__ filename num w


parseClassVarDec :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseClassVarDec [classVarDec_kw,classVarDec_type:xs] filename num w
| not ((classVarDec_kw == "<keyword> static </keyword>\n") || (classVarDec_kw == "<keyword> field </keyword>\n"))  = (True,[classVarDec_kw,classVarDec_type:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<classVarDec>\n" +++ classVarDec_kw +++ classVarDec_type

# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# (ok_parsed_all_sym,xs_,w) = parseAllVars xs filename num w // calls a method that will write to file all the : type varName, type varName ... 

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</classVarDec>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= parseClassVarDec xs_ filename num w
//= (True,xs_,w)

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

parseParameterList :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseParameterList [type,varname,sym:xs] filename num w
| type == "<symbol> ) </symbol>\n" = (True,[type,varname,sym:xs],w)
| sym == "<symbol> ) </symbol>\n" = finishParameterList [type,varname,sym:xs] filename num w

# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")

# string_to_print = type +++ varname +++ sym
# outFile = fwrites string_to_print outFile

# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

| sym == "<symbol> , </symbol>\n" = parseParameterList xs filename num w // until we get a symbol of ;
= (True,xs,w)

finishParameterList :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
finishParameterList [type,varname,sym:xs] filename num w
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = type +++ varname
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,[sym:xs],w)



parseSubroutineBody :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineBody [sym:xs] filename num w //= abort(xs!!0 +++ xs!!1 +++ xs!!2)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<subroutineBody>\n" +++ sym
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

#(ok_vardec,xs_,w) = parseVarDec xs filename num w
| not ok_vardec = abort("failed to parse vardec")

#(ok_stmt,[sym_:xs__],w) = parseStatements xs_ filename num w
| not ok_stmt = abort("failed to parse statements")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = sym_ +++ "</subroutineBody>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
//= parseClassVarDec xs_ filename num w
= (True,xs__,w)


parseVarDec:: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseVarDec [var,type,name:xs] filename num w
| not (var == "<keyword> var </keyword>\n") = (True,[var,type,name:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<varDec>\n" +++ var +++ type +++ name
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_allvars,xs_,w) = parseAllVars xs filename num w
| not ok_allvars = abort("failed vardec")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</varDec>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,xs_,w)
//= parseVarDec xs_ filename num w




/*
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

*/

/************* STATEMENTS ***************/

parseStatements:: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseStatements a b c w = (True,["a"],w)
/*

parseStatement:: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseStatement [first:xs] filename num w
| not (( first == "<keyword> let </keyword>\n") || (first == "<keyword> if </keyword>\n") || (first == "<keyword> while </keyword>\n") || (first == "<keyword> do </keyword>\n") || (first == "<keyword> return </keyword>\n")) = (True,[first:xs],w)
# (ok_let,xs_,w) = parseLetStatement [first:xs] filename w
| not ok_let = abort("error in let statement")
//= (True,xs_,w)
= parseStatement xs_ filename num w

parseLetStatement:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseLetStatement [return:xs] filename w 

parseArrayLetStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseArrayLetStatement [return:xs] filename w 

parseIfStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseIfStatement [return:xs] filename w 



parseElseStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseElseStatement [return:xs] filename w 



parseWhileStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseWhileStatement [return:xs] filename w 




parseDoStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseDoStatement [return:xs] filename w 



parseSubroutineCallNoTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCallNoTerm [x1,x2:xs] filename w// = abort(x1+++x2)


parseReturnStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseReturnStatement [return:xs] filename w 


/************* EXPRESSIONS ***************/

parseExpression :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseExpression [x:xs] filename w


/* allows to write (op term)* */
parseOpTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseOpTerm [op:xs] filename w //= abort("made it till here" +++ op +++ xs!!0)


parseTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTerm [x:xs] filename w //= (True,[x:xs],w)

parseConstant:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseConstant [x:xs] filename w


parseTermExpression:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTermExpression [x:xs] filename w

var_or_subroutine:: [String] String *f -> (Bool,[String],*f) | FileSystem f
var_or_subroutine [x1,x2:xs] filename w

parseVarName:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarName [x1,x2:xs] filename w

parseVarExperssion:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarExperssion [x:xs] filename w

parseSubroutineCall :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCall [x1,x2:xs] filename w// = abort(x1+++x2)


parseNestedSubroutineCall:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseNestedSubroutineCall [dot,name,sym:xs] filename w

parseExpressionList :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseExpressionList [sym:xs] filename w

parseSemiExpression :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSemiExpression [sym:xs] filename w

/*
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
*/
write2file:: String String *f -> (Bool,*f) | FileSystem f
write2file string filename w
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwrites string outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)


getTag:: {#Char} Int -> {#Char}
getTag str len
# strlist = [ c \\ c <-: str ]
= { c \\ c <- ( take len strlist ) }


















