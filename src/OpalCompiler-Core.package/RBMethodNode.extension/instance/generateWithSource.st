*OpalCompiler-Core
generateWithSource
	"Answer a CompiledMethod with source encoded in trailer."

	
	"for doits, we need to store the source pretty printed from the 
	AST to get the return and methodName correct"
	self selector isDoIt ifTrue: [source := self formattedCode].
	
	^self generate: (CompiledMethodTrailer new sourceCode: source).