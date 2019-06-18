implementation module ParserToVM
import StdEnv
import StdFile
import Directory
import FileManipulation
import Tokenizer
import symTable

/*
*  ParseMultipleFiles.
*  foreach file in the files list:
*  1. read the input file to list of lines.
*  2. initial the output file.
*  3. start compilation of the input file, using recursion on the list of the file lines.
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
# outFile = "vmFiles\\" +++ filename +++ ".vm"
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
*  parse:
*  1. cast the line form String to list of Chars.
*  2. and start looking for the first rule.
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
// initialize the class symbol table and counter
# (ok_init1,w) = overrideFile "" "symtables\\classtable.txt" w
| not ok_init1 = abort("failed to intialize class symbol table")
# (ok_init2,w) = overrideFile "0" "symtables\\classstaticcounter.txt" w
| not ok_init2 = abort("failed to intialize class counter")
# (ok_init3,w) = overrideFile "0" "symtables\\classfieldcounter.txt" w
| not ok_init3 = abort("failed to intialize class counter") 
// initialize the 'if' and the 'while' counters
# (ok_init4,w) = overrideFile "0" "counters\\ifcounter.txt" w
| not ok_init4 = abort("failed to intialize if counter")
# (ok_init5,w) = overrideFile "0" "counters\\whilecounter.txt" w
| not ok_init5 = abort("failed to intialize while counter")
//enter fields and statics to the class symbol table.
#(ok_parse_classVarDec,xs_,w) = parseClassVarDec xs filename num w
| not ok_parse_classVarDec = abort("failed to parse vardec")
// generates vm code based on class subroutines.
#(ok_parse_subroutine,xs__,w) = parseSubroutineDec xs_ filename num w
| not ok_parse_subroutine = abort("failed to parse subroutine")
//= (True,w)

parseSubroutineDec :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineDec [subroutine_kw, subroutine_type ,subroutine_name, sym:xs] filename num w
// initialize the method symbol table and counter.
# (ok_init1,w) = overrideFile "" "symtables\\methodtable.txt" w
| not ok_init1 = abort("failed to intialize method symbol table")
# (ok_init2,w) = overrideFile "0" "symtables\\methodcounter.txt" w
| not ok_init2 = abort("failed to intialize method counter") 
// enter arguments to the method symbol table.
# (ok_params,[sym_:xs_],w) = parseParameterList xs filename num w
| not ok_params = abort("failed to parse params list")
// enter locals to the method symbol table, and generate vm code.
# (ok_body,xs__,w) = parseSubroutineBody xs_ filename (getTokenValue subroutine_name) subroutine_kw w
| not ok_body = abort("failed to parse subroutine body")
= parseSubroutineDec xs__ filename num w


parseClassVarDec :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseClassVarDec [kw,type,name,sym:xs] filename num w //= abort(kw +++ type +++ xs!!0 +++ xs!!1 +++ xs!!2)
// end condition:
| not ((kw == "<keyword> static </keyword>\n") || (kw == "<keyword> field </keyword>\n"))  = (True,[kw,type,name,sym:xs],w)
// parse multiple vars in file.
# (ok_parsed,xs_,w) = parseAllClassVars [name,sym:xs] (getTokenValue type) (getTokenValue kw) w // calls a method that will write to file all the : type varName, type varName ... 
| not ok_parsed = abort("failed in parseClassVarDec")
// recursive call
= parseClassVarDec xs_ filename num w
//= (True,xs_,w)

parseAllClassVars:: [String] String String *f -> (Bool,[String],*f) | FileSystem f
parseAllClassVars [name,sym:xs] type kind w
# (okw,w) = insertRecordClassTable (getTokenValue name) type kind w
| sym == "<symbol> ; </symbol>\n" = (True,xs,w)
= parseAllClassVars xs type kind w

/* parses all the symicollons in ClassVarDec */
parseAllVars :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseAllVars [var,type,name,sc:xs] filename w //= abort(var +++ type +++ name +++ sc +++ xs!!0 +++ xs!!1 +++ xs!!2 +++ xs!!3 +++ xs!!4 +++ xs!!5) 
// end condition:
| not (var == "<keyword> var </keyword>\n") = (True,[var,type,name,sc:xs],w)
// insert local to the method symbol table:
# (ok_local,w) = insertRecordMethodTable (getTokenValue name) (getTokenValue type) "local" w
// recursive call:
| sc == "<symbol> , </symbol>\n"  = parseMultipleVarsInLine xs (getTokenValue type) filename w
= parseAllVars xs filename w

parseMultipleVarsInLine:: [String] String String *f -> (Bool,[String],*f) | FileSystem f
parseMultipleVarsInLine [name,sc:xs] type filename w 
| not ((sc == "<symbol> , </symbol>\n")||(sc == "<symbol> ; </symbol>\n")) = (True,[name,sc:xs],w)
// insert local to the method symbol table:
# (ok_local,w) = insertRecordMethodTable (getTokenValue name) type "local" w
// recursive call:
| sc == "<symbol> , </symbol>\n"  = parseMultipleVarsInLine xs type filename w
| sc == "<symbol> ; </symbol>\n" = parseAllVars xs filename w


parseParameterList :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
parseParameterList [type,varname,sym:xs] filename num w
// end conditions -
// the parameter list is empty:
| type == "<symbol> ) </symbol>\n" = (True,[type,varname,sym:xs],w)
// final parameter in list:
| sym == "<symbol> ) </symbol>\n" = finishParameterList [type,varname,sym:xs] filename num w
// append new 'arg' to the method symbol table, automatically updates the counter value.
# (ok_arg,w) = insertRecordMethodTable (getTokenValue varname) (getTokenValue type) "argument" w
| not ok_arg = abort("failed to insert argument symbol")
// recursive call
| sym == "<symbol> , </symbol>\n" = parseParameterList xs filename num w // until we get a symbol of ;
= (True,xs,w)

finishParameterList :: [String] String Int *f -> (Bool,[String],*f) | FileSystem f
finishParameterList [type,varname,sym:xs] filename num w
// insert the last parameter:
# (ok_arg,w) = insertRecordMethodTable (getTokenValue varname) (getTokenValue type) "argument" w
| not ok_arg = abort("failed to insert argument symbol")
= (True,[sym:xs],w)

parseSubroutineBody :: [String] String String String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineBody [sym:xs] filename methodname subroutine_kw w //= abort(sym +++ xs!!0 +++ xs!!1 +++ xs!!2)
// insert locals to the method symbol table:
#(ok_vardec,xs_,w) = parseVarDec xs filename w
| not ok_vardec = abort("failed to parse vardec")
// get the method symbol table counter, indicates the number of locals:
# (ok_locals,num_of_locals,w) = getMethodTableCounter w
| not ok_locals = abort("failed to get number of locals")
// print the 'function' command to the vm file:
# (okw,w) = write2file ("function " +++ filename +++ "." +++ methodname +++ num_of_locals +++ "\n") filename w
| not ok_locals = abort("failed to print 'function' instruction")
// if the subroutine is method, write method prefix:
# (ok_prefix,w) = parseMethodPrefix subroutine_kw filename w
// generate vm code for the function statements:
#(ok_stmt,[sym_:xs__],w) = parseStatements xs_ filename w
| not ok_stmt = abort("failed to parse statements")
= (True,xs__,w)

parseMethodPrefix:: String String *f -> (Bool,*f) | FileSystem f
parseMethodPrefix kw filename w
| not (kw == "<keyword> method </keyword>\n") = (True,w)
# (okw,w) = write2file "push argument 0\npop pointer 0\n" filename w
| not okw = abort("failed to print method prefix")
= (True,w)

parseVarDec:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarDec [var,type,name:xs] filename w
// reset the method symbol table counter:
# (ok_init,w) = overrideFile "0" "symtables\\methodcounter.txt" w
// insert locals to method symbol table:
# (ok_allvars,xs_,w) = parseAllVars [var,type,name:xs] filename w
| not ok_allvars = abort("failed vardec")
= (True,xs_,w)



/************* STATEMENTS ***************/

parseStatements:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseStatements [first:xs] filename w //= abort("hey " +++ first)
// end condition:
| not ((first == "<keyword> let </keyword>\n") || (first == "<keyword> if </keyword>\n") || (first == "<keyword> while </keyword>\n") || (first == "<keyword> do </keyword>\n") || (first == "<keyword> return </keyword>\n")) = (True,[first:xs],w)
// generate vm code for singel statement:
# (ok_stmts,xs_,w) = parseStatement [first:xs] filename w
| not ok_stmts = abort("failed statments")
// not sure if needed, maybe just 'return true' was enough. But I'm not gonna mess with Targil4 structure.
= parseStatement xs_ filename w


parseStatement:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseStatement [first:xs] filename w
// end condition:
| not (( first == "<keyword> let </keyword>\n") || (first == "<keyword> if </keyword>\n") || (first == "<keyword> while </keyword>\n") || (first == "<keyword> do </keyword>\n") || (first == "<keyword> return </keyword>\n")) = (True,[first:xs],w)
// switch control flow to generate vm code to the right kind of statment (we start with 'let' and switch to other cases if needed:)
# (ok_let,xs_,w) = parseLetStatement [first:xs] filename w
| not ok_let = abort("error in let statement")
= parseStatement xs_ filename w

parseLetStatement:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseLetStatement [first,name,sym:xs] filename w //= abort(first +++ name +++ sym +++ xs!!0)
| not (first == "<keyword> let </keyword>\n") = parseIfStatement [first,name,sym:xs] filename w

| sym == "<symbol> [ </symbol>\n" = parseArrayLetStatement [name,sym:xs] filename w

// sc - semi colmun (;)
# (ok_expr,[sc:xs_],w) = parseExpression xs filename w
| not ok_expr = abort("failed to gen code for expression")
// pop the result from parseExpression into the left-side-variable
# symbol_name = getTokenValue name
# (ok_fetch,index,kind,w) = fetchVariableFromTables symbol_name w
| not ok_fetch = abort("failed to get symbol record fields")
# (ok_right,w) = write2file ("pop " +++ kind +++ " " +++ index +++ "\n") filename w
| not ok_right = abort("failed to write vm code to file")
//abort(sc +++ xs_!!0 +++ xs_!!1 +++ xs_!!2)
= (True,xs_,w)


parseArrayLetStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseArrayLetStatement [name,sym:xs] filename w //= abort(name +++ sym +++ xs!!0)// varName[expression1] = expression2
// parse expression1
# (ok_exp1,[soger,asgn:xs_],w) = parseExpression xs filename w
| not ok_exp1 = abort("abort in parseArrayLetStatement exp1")
// parse varName,  varName+expression1
# symbol_name = getTokenValue name
# (ok_fetch,index,kind,w) = fetchVariableFromTables symbol_name w
| not ok_fetch = abort("failed to get symbol record fields")
# (ok_write,w) = write2file ("push " +++ kind +++ " " +++ index +++ "\nadd\n") filename w
| not ok_write = abort("abort in parseArrayLetStatement varName")
// parse expression2
# (ok_exp2,[sc:xs__],w) = parseExpression xs_ filename w
| not ok_exp2 = abort("abort in parseArrayLetStatement exp2")
// array suffix:
# (ok_write2,w) = write2file "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n" filename w
// return:
= (True,xs__,w)


parseIfStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseIfStatement [first,opening:xs] filename w //= (True,[return:xs],w)
| not (first == "<keyword> if </keyword>\n") = parseWhileStatement [first,opening:xs] filename w
// parse the 'if' condition (expression)

// if the condition satisfied - goto IF_TRUE label, else goto IF_FALSE

// IF_TRUE label

// parse the 'if' statements

// if we found 'else' token, switch to 'parseElseStatement'

// IF_FALSE label



parseElseStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseElseStatement [return:xs] filename w 
// if the condition was satisfied - goto IF_END

// IF_FALSE label

// parse the 'else' statements

// IF_END label


parseWhileStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseWhileStatement [first,opening:xs] filename w
| not (first == "<keyword> while </keyword>\n") = parseDoStatement [first,opening:xs] filename w
//print WHILE_EXP# label

// parse the 'while' condition (expression)
 
// if the condition is not satisfied - goto WHILE_END label

// parse the 'while' statements

// goto WHILE_EXP

// WHILE_END label
 
 
 
parseDoStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseDoStatement [first,name,sym:xs] filename w 
| not (first == "<keyword> do </keyword>\n") = parseReturnStatement [first,name,sym:xs] filename w 
# (ok_sr,[sc:xs_],w) = parseSubroutineCall [name,sym:xs] filename w
| not ok_sr = abort("abort in parseDoStatement")
# (okw,w) = write2file "pop temp 0\n" filename w
| not okw = abort("abort in parseDoStatement")
= (True,xs_,w)

parseSubroutineCall:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCall [name,sym:xs] filename w //= abort("H" +++ name+++sym+++xs!!0)
// of there is dot, it can be external function or method
| sym == "<symbol> . </symbol>\n" = doExternalSubroutine [name,sym:xs] filename w 
// if there is no dot so its have to be *method* from this class.
// initial method calling by pushing 'pointer 0' into stack.
# (okw,w) = write2file "push pointer 0\n" filename w
// push arguments into stack.
# (okp,count,tokens,w) = pushArguments xs 1 filename w
| not okp = abort("failed")
// call method
# (okw2,w) = write2file ("call " +++ filename +++ "." +++ (getTokenValue name) +++ (toString count) +++ "\n") filename w
// return
//= abort(tokens!!0 +++ tokens!!1 +++ tokens!!2 +++ tokens!!3 +++ tokens!!4)
= (True,tokens,w)



doExternalSubroutine :: [String] String *f -> (Bool,[String],*f) | FileSystem f
doExternalSubroutine [class_name,dot,name,sym:xs] filename w //= abort(class_name +++ dot +++ name +++sym)
// if 'class_name' is not in the symtables, it means the Subroutine is function and not method.
# (objExist,s1,s2,w) = fetchVariableFromTables (getTokenValue class_name) w
//= abort(toString objExist)
| not objExist = doExternalFunction [class_name,dot,name,sym:xs] filename w
// initial method calling by pushing 'pointer 0' into stack.
# (okw,w) = write2file "push pointer 0\n" filename w
// push arguments into stack.
# (okp,count,tokens,w) = pushArguments xs 1 filename w
| not okp = abort("failed")
// call method
# class_list = getTokenValue class_name
# len = length [ c \\ c <-: class_list ]
# classname =  { c \\ c <- (take (len-1) [ c \\ c <-: class_list]) }
# (okw2,w) = write2file ("call " +++ classname +++ "." +++ (getTokenValue name) +++ (toString count) +++ "\n") filename w
// return
//= abort(tokens!!0 +++ tokens!!1 +++ tokens!!2 +++ tokens!!3 +++ tokens!!4)
= (True,tokens,w)

doExternalFunction :: [String] String *f -> (Bool,[String],*f) | FileSystem f
doExternalFunction [class_name,dot,name,sym:xs] filename w
// push arguments into stack.
//= abort("H" +++ xs!!0 +++ xs!!1 +++ xs!!2)
# (okp,count,tokens,w) = pushArguments xs 0 filename w
| not okp = abort("failed")
// call method
# class_list = getTokenValue class_name
# len = length [ c \\ c <-: class_list ]
# classname =  { c \\ c <- (take (len-1) [ c \\ c <-: class_list]) }
# (okw2,w) = write2file ("call " +++ classname +++ "." +++ (getTokenValue name) +++ (toString count) +++ "\n") filename w
// return
//= abort( sc +++ tokens!!0 +++ tokens!!1 +++ tokens!!2 +++ tokens!!3 +++ tokens!!4)
= (True,tokens,w)

pushArguments:: [String] Int String *f -> (Bool,Int,[String],*f) | FileSystem f
// if we have ')' in the begining of the input, we reach the end of the subroutine parameters.
pushArguments ["<symbol> ) </symbol>\n":xs] count filename w = (True,count,xs,w)
// otherwise, parseExpression pushs the argument into stack.
pushArguments input count filename w //= abort(input!!0 +++ input!!1 +++ input!!2 +++ input!!3 +++ input!!4)
# (ok_exp,[sym:input_],w) = parseExpression input filename w
| not ok_exp = abort("failed to parse expression in push argument")
| sym == "<symbol> , </symbol>\n" = pushArguments input_ (count+1) filename w
= pushArguments [sym:input_] (count+1) filename w



/*parseSubroutineCallNoTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseSubroutineCallNoTerm [x1,x2:xs] filename w = abort(x1+++x2)*/


parseReturnStatement :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseReturnStatement [return,sym:xs] filename w //= abort(return+++sym+++xs!!0)
// if its just 'return;':
| sym == "<symbol> ; </symbol>\n" = retrunVoid [return,sym:xs] filename w 
// generate vm code for the returned expression.
# (ok_stmts,[sym:xs_],w) = parseExpression [sym:xs] filename w
| not ok_stmts = abort("failed returnStatement")
# (okw,w) = write2file ("return\n") filename w
= (True,xs_,w)

retrunVoid:: [String] String *f -> (Bool,[String],*f) | FileSystem f
retrunVoid [return,sym:xs] filename w
# (okw,w) = write2file "push constant 0\nreturn\n" filename w
| not okw = abort("failed return void")
= (True,xs,w)

/************* EXPRESSIONS ***************/

parseExpression :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseExpression [x:xs] filename w
| (x == "<symbol> ; </symbol>\n") = (True,[x:xs],w)
// generate vm code for term
# (ok_stmts,[op:xs_],w) = parseTerm [x:xs] filename w
| not ok_stmts = abort("failed to generate code for term")
// generate vm code for (op term)*
# (ok_stmts,xs__,w) = parseOpTerm [op:xs_] filename w
| not ok_stmts = abort("failed expression")
//= abort(xs__!!0 +++ xs__!!1 +++ xs__!!2 +++ xs__!!3)
= (True,xs__,w)

/* allows to write (op term)* */
parseOpTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseOpTerm [op:xs] filename w //= abort("made it till here" +++ op +++ xs!!0)
# plus = "<symbol> + </symbol>\n"
# sub  = "<symbol> - </symbol>\n"
# mult = "<symbol> * </symbol>\n"
# div  = "<symbol> / </symbol>\n"
# asgn = "<symbol> = </symbol>\n"
# amp  = "<symbol> &amp; </symbol>\n"
# or   = "<symbol> | </symbol>\n"
# lt   = "<symbol> &lt; </symbol>\n"
# gt   = "<symbol> &gt; </symbol>\n"
| not ((op == plus) || (op == sub) ||(op == mult) ||(op == div) ||(op == asgn) ||(op == amp) ||(op == or) ||(op == lt) ||(op == gt)) = (True,[op:xs],w)
// generate vm code for term:
# (ok_stmts,xs_,w) = parseTerm xs filename w
| not ok_stmts = abort("failed expression")

// generate vm instruction based on 'op':
# (okw,w) = write2file ((opName op)+++"\n") filename w
= parseOpTerm xs_ filename w


parseTerm :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTerm [x:xs] filename w //= abort(x +++ xs!!0 +++ xs!!1 +++ xs!!2)//= (True,[x:xs],w)
| ((getTag x 17) == "<integerConstant>") = parseIntegerConstant [x:xs] filename w
| ((getTag x 16) == "<stringConstant>")  = parseStringConstant [x:xs] filename w
//| ((getTag x 9) == "<keyword>")  = parseConstant [x:xs] filename w
| xs!!0 == "<symbol> [ </symbol>\n" = parseArrayTerm [x:xs] filename w
| ((x == "<symbol> - </symbol>\n") || (x == "<symbol> ~ </symbol>\n"))  = parseUnaryOp [x:xs] filename w
| (x == "<symbol> ( </symbol>\n") = parseTermExpression [x:xs] filename w
| ((getTag x 12) == "<identifier>")  = var_or_subroutine [x:xs] filename w
= abort("ABORT\n" +++ x +++ xs!!0 +++ xs!!1)

parseStringConstant:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseStringConstant [x:xs] filename w
// get string length:
# string = getTokenValue x
# charlist = [ c \\ c <-: string ]
# len = length charlist
# (okw,w) = write2file  ("push constant " +++ (toString (len-1)) +++ "\n") filename w
// call String.new
# (okw2,w) = write2file "call String.new 1\n" filename w
// recursivly call String.appendChar
# (okchars,w) = parseAppendChar charlist filename w
| not okchars = abort("failed to append chars to string")
//= abort(xs!!0 +++ xs!!1 +++ xs!!2 +++ xs!!3 +++ xs!!4)
= (True,xs,w)

parseAppendChar:: [Char] String *f -> (Bool,*f) | FileSystem f
// the string always end with extra space after the last char.
parseAppendChar [x,' '] filename w
# (okw,w) = write2file ("push constant " +++ (toString (toInt x)) +++ "\ncall String.appendChar 2\n") filename w
| not okw = abort("failed to append char to string")
= (True,w)

parseAppendChar [x:xs] filename w
# (okw,w) = write2file ("push constant " +++ (toString (toInt x)) +++ "\ncall String.appendChar 2\n") filename w
| not okw = abort("failed to append char to string")
= parseAppendChar xs filename w


parseIntegerConstant:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseIntegerConstant [x:xs] filename w
# (okw,w) = write2file ("push constant " +++ (getTokenValue x) +++ "\n") filename w
= (True,xs,w)



var_or_subroutine:: [String] String *f -> (Bool,[String],*f) | FileSystem f
var_or_subroutine [x1,x2:xs] filename w
| x2 == "<symbol> ( </symbol>\n" = parseSubroutineCall [x1,x2:xs] filename w
| x2 == "<symbol> . </symbol>\n" = parseSubroutineCall [x1,x2:xs] filename w
= parseVarName [x1,x2:xs] filename w


parseVarName:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarName [x1,x2:xs] filename w //= abort(x1 +++ x2 +++ xs!!0)
//| x2 == "<symbol> [ </symbol>\n" = parseVarExperssion [x2:xs] filename w

// push var to stack:
# symbol_name = getTokenValue x1
# (ok_fetch,index,kind,w) = fetchVariableFromTables symbol_name w
| not ok_fetch = abort("failed to get symbol record fields")
# (ok_right,w) = write2file ("push " +++ kind +++ " " +++ index +++ "\n") filename w
| not ok_right = abort("failed to write vm code to file")
= (True,[x2:xs],w)

// parseSubroutineCall is not here. GOTO parseDoStatement.

parseUnaryOp :: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseUnaryOp [op:xs] filename w
# instruction = unaryOpName op
// generate vm code for term
# (okt,tokens,w) = parseTerm xs filename w
# (okw,w) = write2file instruction filename w
= (True,tokens,w)


parseTermExpression:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseTermExpression [x:xs] filename w
// generate vm code for the expression
# (ok_e,[x_:xs_],w) = parseExpression xs filename w
= (True,xs_,w)

parseArrayTerm:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseArrayTerm [varName,open:xs] filename w
// parse expression
# (ok_exp,[close:xs_],w) = parseExpression xs filename w
| not ok_exp = abort("failed in parseArrayTerm 2")
// push varName, and other necessary actions
# symbol_name = getTokenValue varName
# (ok_fetch,index,kind,w) = fetchVariableFromTables symbol_name w
| not ok_fetch = abort("failed to get symbol record fields")
# (ok_write,w) = write2file ("push " +++ kind +++ " " +++ index +++ "\nadd\npop pointer 1\npush that 0\n") filename w
| not ok_write = abort("failed in parseArrayTerm 3")
// return
= (True,xs_,w)


/*



parseVarExperssion:: [String] String *f -> (Bool,[String],*f) | FileSystem f
parseVarExperssion [x:xs] filename w




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
# outFile = "vmFiles\\" +++ filename +++ ".vm"
# (ok_open,outFile,w) = fopen outFile FAppendText w
| not ok_open = abort("failed to open file")
# outFile = fwrites string outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)

overrideFile:: String String *f -> (Bool,*f) | FileSystem f
overrideFile string filename w
# (ok_open,outFile,w) = fopen filename FWriteText w
| not ok_open = abort("failed to open file")
# outFile = fwrites string outFile
# (ok_read_close,w) = fclose outFile w
| not ok_read_close = abort("failed to close file")
= (True,w)


incIfCounter:: *f -> (Bool,Int,*f) | FileSystem f
incIfCounter w
// open the file for reading, get the counter, close the file:
# cFile = "counters\\ifcounter.txt"
# filename = "counters\\ifcounter.txt"
# (okopen,cFile,w) = fopen cFile FReadText w
| not okopen = abort("")
# (counterstr,cFile) = freadline cFile
# counter = toInt counterstr
# (okclose,w) = fclose cFile w
// override the file with the new counter value.
# (ok1,w) = overrideFile (toString (counter+1)) filename w
| not ok1 = abort("failed to increase if counter")
= (True,counter,w)


incWhileCounter:: *f -> (Bool,Int,*f) | FileSystem f
incWhileCounter w
// open the file for reading, get the counter, close the file:
# cFile = "counters\\whilecounter.txt"
# filename = "counters\\ifcounter.txt"
# (okopen,cFile,w) = fopen cFile FReadText w
| not okopen = abort("")
# (counterstr,cFile) = freadline cFile
# counter = toInt counterstr
# (okclose,w) = fclose cFile w
// override the file with the new counter value.
# (ok1,w) = overrideFile (toString (counter+1)) filename w
| not ok1 = abort("failed to increase if counter")
= (True,counter,w)

getTag:: {#Char} Int -> {#Char}
getTag str len
# strlist = [ c \\ c <-: str ]
= { c \\ c <- ( take len strlist ) }

getTokenValue:: {#Char} -> {#Char}
getTokenValue str
# taglen = tagLength [ c \\ c <-: str ] 0
# strlist = [ c \\ c <-: str ]
# strlen  = length strlist 					// one for '\n', anohter one for the extra char in closing tag, and the last for the space.
= { c \\ c <- (drop (taglen+1) ( take (strlen - taglen - 2) strlist ) ) }


tagLength:: [Char] Int -> Int
tagLength [x:xs] acc
| x == '>' = (acc+1)
= tagLength xs (acc+1)




opName:: String -> String
opName op
| op == "<symbol> + </symbol>\n"		= "add"
| op == "<symbol> - </symbol>\n"		= "sub"
| op == "<symbol> * </symbol>\n"		= "call Math.multiply 2"
| op == "<symbol> / </symbol>\n"		= "call Math.divide 2"
| op == "<symbol> = </symbol>\n"		= "eq"
| op == "<symbol> &amp; </symbol>\n"	= "and"
| op == "<symbol> | </symbol>\n"		= "or"
| op == "<symbol> &lt; </symbol>\n"		= "lt"
| op == "<symbol> &gt; </symbol>\n"		= "gt"

unaryOpName::String -> String
unaryOpName op
| op == "<symbol> ~ </symbol>\n"		= "not\n"
| op == "<symbol> - </symbol>\n"		= "neg\n"






