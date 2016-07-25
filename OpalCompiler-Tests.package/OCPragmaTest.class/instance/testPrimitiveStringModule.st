testing
testPrimitiveStringModule
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPrimitiveStringModule.
	
	self assert: (aRBMethode compiledMethod primitive = 117)