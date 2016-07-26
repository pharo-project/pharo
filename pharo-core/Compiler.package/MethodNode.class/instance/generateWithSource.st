code generation
generateWithSource
	"Answer a CompiledMethod with source encoded in trailer."
	
	| source |
	
	"for doits, we need to store the source pretty printed from the 
	AST to get the return and methodName correct"
	self selector isDoIt ifTrue: [sourceText := self printString].
	
	^self generate: (CompiledMethodTrailer new sourceCode: sourceText).