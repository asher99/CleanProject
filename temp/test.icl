module test
import StdEnv

getTag:: {#Char} Int -> {#Char}
getTag str len
# strlist = [ c \\ c <-: str ]
= { c \\ c <- ( take len strlist ) }


Start
# str = "<tag> hello </tag>"
= getTag str 5