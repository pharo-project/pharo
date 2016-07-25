testing
testPrimitiveString
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPrimitiveString.
	
	self assert: (aRBMethode compiledMethod primitive = 117)