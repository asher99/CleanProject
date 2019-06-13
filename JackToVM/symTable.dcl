definition module symTable
import StdEnv
import StdFile
import Directory

insertRecordClassTable:: String String String *f -> (Bool,*f) | FileSystem f

insertRecordMethodTable:: String String String *f -> (Bool,*f) | FileSystem f

getIndexClassTable:: String *f -> (Bool,String,*f) | FileSystem f

getIndexMethodTable:: String *f -> (Bool,String,*f) | FileSystem f

getMethodTableCounter:: *f -> (Bool,String,*f) | FileSystem f

getClassTableCounter:: *f -> (Bool,String,*f) | FileSystem f

