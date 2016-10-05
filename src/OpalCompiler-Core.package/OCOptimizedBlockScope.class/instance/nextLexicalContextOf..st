lookup
nextLexicalContextOf: aContext

	"Returns the next context to lookup a variable name from within this Scope.
	A method should continue looking up a variable from the outerContext.
	
	An inlined BlockClosure next lexical context is the same context, since no new context was created for this block closure"
	^ aContext