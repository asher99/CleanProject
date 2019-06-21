definition module symTable
import StdEnv
import StdFile
import Directory

insertRecordClassTable:: String String String *f -> (Bool,*f) | FileSystem f

insertRecordMethodTable:: String String String *f -> (Bool,*f) | FileSystem f

getIndexClassTable:: String *f -> (Bool,String,*f) | FileSystem f

getMethodSymbolIndex:: String *f -> (Bool,String,*f) | FileSystem f

getMethodSymbolKind:: String *f -> (Bool,String,String,*f) | FileSystem f

getMethodTableCounter:: *f -> (Bool,String,*f) | FileSystem f

getClassStaticTableCounter:: *f -> (Bool,String,*f) | FileSystem f

getClassFieldTableCounter:: *f -> (Bool,String,*f) | FileSystem f

fetchVariableFromTables:: String *f -> (Bool,String,String,String,*f) | FileSystem f

