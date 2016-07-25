testing
testPragmaAfterBeforTemp
	"self debug: #testPragmaAfterBeforTemp"
	| aRBMethode |
	aRBMethode := OpalCompiler new parse: self methodPragmaAfterBeforTemps.


	self assert: (aRBMethode compiledMethod pragmas first keyword = #hello:).
	self assert: (aRBMethode compiledMethod pragmas second keyword = #world:)
	