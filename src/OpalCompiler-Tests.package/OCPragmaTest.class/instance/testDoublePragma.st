testing
testDoublePragma
	| aRBMethode |
	"self debug: #testDoublePragma"
	aRBMethode := OpalCompiler new parse: self methodDoublePragma.
	
	self assert: (aRBMethode compiledMethod pragmas first keyword = #hello:).
	self assert: (aRBMethode compiledMethod pragmas second keyword = #hello:)