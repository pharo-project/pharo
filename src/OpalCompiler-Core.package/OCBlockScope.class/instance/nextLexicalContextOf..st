lookup
nextLexicalContextOf: aContext

	"Returns the next context to lookup a variable name from within this Scope.
	A method should continue looking up a variable from the outerContext.
	
	A BlockClosure's next lexical context is the context where it was created from, and it can access variables from it."
	^ aContext home