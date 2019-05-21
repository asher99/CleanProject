definition module Tokenizer
import StdEnv
import StdFile

TokenizeMultipleFiles:: [String] *f -> (Bool,*f) | FileSystem f

Tokenize:: [String] String Int *f -> (Bool,*f) | FileSystem f

FDAutomaton_state_0:: [Char] String Int *f -> (Bool,*f) | FileSystem f

FDAutomaton_transit_0_5:: [Char] String Int *f -> (Bool,*f) | FileSystem f

FDAutomaton_state_5:: [Char] String Int *f -> (Bool,*f) | FileSystem f

FDAutomaton_transit_5_6:: [Char] String Int *f -> (Bool,*f) | FileSystem f

FDAutomaton_state_6:: [Char] String Int *f -> (Bool,*f) | FileSystem f
