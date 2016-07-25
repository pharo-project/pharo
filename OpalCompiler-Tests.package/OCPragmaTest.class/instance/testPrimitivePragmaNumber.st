testing
testPrimitivePragmaNumber
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPrimitivePragma.
	self assert: (aRBMethode compiledMethod primitive = 4)