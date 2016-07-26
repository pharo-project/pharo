testing
testPragmaTwoParam
	"self debug: #testPragmaAfterBeforTemp"
	| aRBMethode |
	aRBMethode := OpalCompiler new parse: self methodPragmaTwoParam.


	self assert: (aRBMethode compiledMethod pragmas first keyword = #hello:by:)
	