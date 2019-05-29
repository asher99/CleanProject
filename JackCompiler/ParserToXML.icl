implementation module ParserToXML
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
parseStatements [first:xs] filename num w //= abort(first +++ a +++ b +++ c)
| not ((first == "<keyword> let </keyword>\n") || (first == "<keyword> if </keyword>\n") || (first == "<keyword> while </keyword>\n") || (first == "<keyword> do </keyword>\n") || (first == "<keyword> return </keyword>\n")) = (True,[first:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<statements>\n"
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_stmts,xs_,w) = parseStatement [first:xs] filename num w
| not ok_stmts = abort("failed statments")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</statements>\n"
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
//= (True,xs_,w)
= parseStatement xs_ filename num w

parseStatement:: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseStatement [first:xs] filename num w
| not (( first == "<keyword> let </keyword>\n") || (first == "<keyword> if </keyword>\n") || (first == "<keyword> while </keyword>\n") || (first == "<keyword> do </keyword>\n") || (first == "<keyword> return </keyword>\n")) = (True,[first:xs],w)
# (ok_let,xs_,w) = parseLetStatement [first:xs] filename w
| not ok_let = abort("error in let statement")
//= (True,xs_,w)
= parseStatement xs_ filename num w

parseLetStatement:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseLetStatement [first,name,sym:xs] filename w //= abort(first +++ name +++ sym)
| not (first == "<keyword> let </keyword>\n") = parseIfStatement [first,name,sym:xs] filename w
# string2print = "<letStatement>\n" +++ first +++ name +++ sym
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwrites string2print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

| sym == "<symbol> [ </symbol>\n" = parseArrayLetStatement xs filename w
// sc - semi colmun (;)
# (ok_expr,[sc:xs_],w) = parseExpression xs filename w
# string2print2 = sc +++ "</letStatement>\n"

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# outFile2 = fwrites string2print2 outFile2
# (ok_read_close,w) = fclose outFile2 w
| not ok_read_close = abort("failed to close file")

//# (ok_st,xs__,w) = parseStatement xs_ filename w
//| not ok_st = abort("")
= (True,xs_,w)


parseArrayLetStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseArrayLetStatement input filename w
# (ok_expr1,[closing,assign:xs],w) = parseExpression input filename w
# string2print = closing +++ assign
# (ok_w,w) = write2file string2print filename w
| not ok_w = abort("error in array let statemeny")
// sc - semi colmun (;)
# (ok_expr2,[sc:xs_],w) = parseExpression xs filename w
# string2print2 = sc +++ "</letStatement>\n"
# (ok_tag2,w) = write2file string2print filename w
| not ok_tag2 = abort("error in let statemeny")
= (True,xs_,w)

parseIfStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseIfStatement [first,opening:xs] filename w //= abort(first +++opening +++ xs!!0)
| not (first == "<keyword> if </keyword>\n") = parseWhileStatement [first,opening:xs] filename w
# string2print = "<ifStatement>\n" +++ first +++ opening
# (ok_w,w) = write2file string2print filename w
| not ok_w = abort("error in if statement")

# (ok_expr,[closing,open_stmt:xs_],w) = parseExpression xs filename w
# (ok_w,w) = write2file (closing +++ open_stmt) filename w
| not ok_w = abort("error in if statement")

# (ok_stmt,[clos_stmt,x:xs__],w) = parseStatements xs_ filename 0 w
# (ok_w,w) = write2file clos_stmt filename w

| x == "<keyword> else </keyword>\n" = parseElseStatement xs_ filename w

# (ok_tag2,w) = write2file "</ifStatement>\n" filename w
| not ok_tag2 = abort("error in let statemeny")
= (True,xs_,w)


parseElseStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseElseStatement [else,opening:xs] filename w
# string2print = else +++ opening
# (ok_w,w) = write2file string2print filename w
| not ok_w = abort("error in if statement")

# (ok_expr,[closing,open_stmt:xs_],w) = parseExpression xs filename w
# (ok_w,w) = write2file (closing +++ open_stmt) filename w
| not ok_w = abort("error in if statement")

# (ok_stmt,[clos_stmt,x:xs_],w) = parseStatements xs filename 0 w
# (ok_w,w) = write2file clos_stmt filename w

# (ok_tag2,w) = write2file "</ifStatement>\n" filename w
| not ok_tag2 = abort("error in let statemeny")
= (True,xs_,w)


parseWhileStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseWhileStatement [first,opening:xs] filename w
| not (first == "<keyword> while </keyword>\n") = parseDoStatement [first,opening:xs] filename w

# string2print = "<whileStatement?\n" +++ first +++ opening
# (ok_w,w) = write2file string2print filename w
| not ok_w = abort("error in if statement")

# (ok_stmt,[clos_stmt,x:xs_],w) = parseStatements xs filename 0 w
# (ok_w,w) = write2file clos_stmt filename w

# (ok_tag2,w) = write2file "</ifStatement>\n" filename w
| not ok_tag2 = abort("error in let statemeny")
= (True,xs_,w)




parseDoStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseDoStatement [first:xs] filename w //= abort(first +++ xs!!0)
| not (first == "<keyword> do </keyword>\n") = parseReturnStatement [first:xs] filename w 
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<doStatement>\n" +++ first
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_stmts,[sym:xs_],w) = parseSubroutineCallNoTerm xs filename w
| not ok_stmts = abort("failed doStatmente")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = sym +++ "</doStatement>\n" // symbol here is ;
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,xs_,w)

parseSubroutineCallNoTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCallNoTerm [x1,x2:xs] filename w// = abort(x1+++x2)
# (ok_w,w) = write2file x1 filename w
| not ok_w = abort("a")
| x2 == "<symbol> . </symbol>\n" = parseNestedSubroutineCall [x2:xs] filename w
# (ok_w,w) = write2file x2 filename w
| not ok_w = abort("a")
# (ok_w,w) = write2file "<expressionList>\n" filename w
# (ok_e,[x:xs_],w) = parseExpressionList xs filename w
# (ok_w,w) = write2file ("</expressionList>\n" +++ x) filename w
| not ok_w = abort("a")
= (True,xs_,w)


parseReturnStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseReturnStatement [return:xs] filename w 
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<returnStatement>\n" +++ return
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_stmts,[sym:xs_],w) = parseExpression xs filename w
| not ok_stmts = abort("failed returnStatement")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = sym +++ "</returnStatement>\n" // symbol here is ;
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,xs_,w)


/************* EXPRESSIONS ***************/

parseExpression :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseExpression [x:xs] filename w
| (x == "<symbol> ; </symbol>\n") = (True,[x:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<expression>\n" 
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_stmts,[op:xs_],w) = parseTerm [x:xs] filename w
//| ((op == "<symbol> + </symbol>\n") || (op == "<symbol> - </symbol>\n") || (op == "<symbol> * </symbol>\n") || (op == "<symbol> / </symbol>\n") || (op == "<symbol> &amp; </symbol>\n")) =  parseOpTerm [op:xs_] filename w
//| ((op == "<symbol> | </symbol>\n") || (op == "<symbol> &gt; </symbol>\n") || (op == "<symbol> &lt; </symbol>\n") || (op == "<symbol> = </symbol>\n")) = parseOpTerm [op:xs_] filename w

// calls to a method who does: (op term)*
# (ok_stmts,xs__,w) = parseOpTerm [op:xs_] filename w
| not ok_stmts = abort("failed expression")

# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</expression>\n" 
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")
= (True,xs__,w)

/* allows to write (op term)* */
parseOpTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseOpTerm [op:xs] filename w //= abort("made it till here" +++ op +++ xs!!0)
| not((op == "<symbol> + </symbol>\n") || (op == "<symbol> - </symbol>\n")||(op == "<symbol> * </symbol>\n") || (op == "<symbol> / </symbol>\n")) = (True,[op:xs],w)
| not((op == "<symbol> = </symbol>\n") || (op == "<symbol> &amp </symbol>\n") || (op == "<symbol> | </symbol>\n") || (op == "<symbol> &lt </symbol>\n") || (op == "<symbol> &gt </symbol>\n")) = (True,[op:xs],w)

# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
//# string_to_print = op 
# outFile = fwrites op outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
# (ok_stmts,xs_,w) = parseTerm xs filename w
| not ok_stmts = abort("failed expression")
= parseOpTerm xs_ filename w



parseTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTerm [x:xs] filename w //= (True,[x:xs],w)
| ((getTag x 17) == "<integerConstant>") = parseConstant [x:xs] filename w
| ((getTag x 16) == "<stringConstant>")  = parseConstant [x:xs] filename w
| ((getTag x 9) == "<keyword>")  = parseConstant [x:xs] filename w
| ((x == "<symbol> - </symbol>\n") || (x == "<symbol> ~ </symbol>\n"))  = parseConstant [x:xs] filename w
| (x == "<symbol> ( </symbol>\n") = parseTermExpression [x:xs] filename w
| ((getTag x 12) == "<identifier>")  = var_or_subroutine [x:xs] filename w


parseConstant:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseConstant [x:xs] filename w
# (ok,w) = write2file ("<term>\n" +++ x +++ "</term>\n") filename w
| not ok = abort("error constant")
= (True,xs,w)

parseTermExpression:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTermExpression [x:xs] filename w
# string2print = "<term>\n" +++ x
# (ok_w,w) = write2file string2print filename w
| not ok_w = abort("")
# (ok_e,[x_:xs_],w) = parseExpression xs filename w
# string2print2 = x_ +++ "</term>\n"
# (ok_2,w) = write2file string2print2 filename w
= (True,xs_,w)

var_or_subroutine:: [String] String *f -> (Bool,[String],*f) | FileSystem f
var_or_subroutine [x1,x2:xs] filename w
| x2 == "<symbol> ( </symbol>\n" = parseSubroutineCall [x1,x2:xs] filename w
| x2 == "<symbol> . </symbol>\n" = parseSubroutineCall [x1,x2:xs] filename w
= parseVarName [x1,x2:xs] filename w

parseVarName:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarName [x1,x2:xs] filename w
# str2prt = "<term>\n" +++ x1
# (ok_w,w) = write2file str2prt filename w
| not ok_w = abort("a")
| x2 == "<symbol> [ </symbol>\n" = parseVarExperssion [x2:xs] filename w
# str2prt2 = "</term>\n"
# (ok_w2,w) = write2file str2prt2 filename w
| not ok_w2 = abort("a2")
= (True,[x2:xs],w)

parseVarExperssion:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarExperssion [x:xs] filename w
# (ok_w,w) = write2file x filename w
# (ok_e,[x_:xs_],w) = parseExpression xs filename w
# (ok_w,w) = write2file (x_ +++ "</term>\n") filename w
= (True,xs_,w)


parseSubroutineCall :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCall [x1,x2:xs] filename w// = abort(x1+++x2)
# str2prt = "<term>\n" +++ x1
# (ok_w,w) = write2file str2prt filename w
| not ok_w = abort("a")
| x2 == "<symbol> . </symbol>\n" = parseNestedSubroutineCall [x2:xs] filename w
# (ok_w,w) = write2file x2 filename w
| not ok_w = abort("a")

# (ok_w,w) = write2file ("<expressionList>\n") filename w
# (ok_e,[x:xs_],w) = parseExpressionList xs filename w
# (ok_w,w) = write2file ("</expressionList>\n" +++ x +++ "</term>\n") filename w
| not ok_w = abort("a")
= (True,xs_,w)

parseNestedSubroutineCall:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseNestedSubroutineCall [dot,name,sym:xs] filename w
# (ok_w,w) = write2file (dot+++name+++sym) filename w
| not ok_w = abort("a")
# (ok_w,w) = write2file ("<expressionList>\n") filename w
# (ok_e,[x_:xs_],w) = parseExpressionList xs filename w
# (ok_w,w) = write2file ("</expressionList>\n" +++ x_ /*+++ "</term>\n"*/) filename w
| not ok_w = abort("a")
= (True,xs_,w)

parseExpressionList :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseExpressionList [sym:xs] filename w
| sym == "<symbol> ) </symbol>\n" = (True,[sym:xs],w)
/*# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = "<expressionList>\n" 
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")*/

# (ok_stmts,xs_,w) = parseExpression [sym:xs] filename w
| not ok_stmts = abort("failed expression")

#(ok_semi,xs__,w) = parseSemiExpression xs_ filename w
| not ok_semi = abort("failed semiEpression")

/*# outFile2 = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open2,outFile2,w) = fopen outFile2 FAppendText w
| not ok_open2 = abort("failed to open file")
# string_to_print_end = "</expressionList>\n" 
# outFile2 = fwrites string_to_print_end outFile2
# (ok_read_close2,w) = fclose outFile2 w
| not ok_read_close2 = abort("failed to close file")*/
= (True,xs__,w)

parseSemiExpression :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSemiExpression [sym:xs] filename w
//| not (sym == "<symbol> , </symbol>\n") = (True,[sym:xs],w)
| (sym == "<symbol> ) </symbol>\n") = (True,[sym:xs],w)
# outFile = "xmlFiles\\" +++ filename +++ ".xml"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# string_to_print = sym
# outFile = fwrites string_to_print outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")

# (ok_stmts,xs_,w) = parseExpression xs filename w
| not ok_stmts = abort("failed expression")

= parseSemiExpression xs_ filename w


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


















