"
The parser does not now the semantic meaning of Variables. It thus sets them all to be an UnresolvedVariable.

The Semantic Analysis (#doSemanticAnalysis) then gives meaning to Variables and sets them correctly.
"
Class {
	#name : #UnresolvedVariable,
	#superclass : #Variable,
	#category : #'OpalCompiler-Core-Semantics'
}
