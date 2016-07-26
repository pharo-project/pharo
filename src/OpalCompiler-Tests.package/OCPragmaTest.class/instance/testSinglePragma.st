testing
testSinglePragma
	| aRBMethode |
	aRBMethode := OpalCompiler new parse: self methodSinglePragma.
	self assert: (aRBMethode compiledMethod pragmas first keyword = #hello:)
	