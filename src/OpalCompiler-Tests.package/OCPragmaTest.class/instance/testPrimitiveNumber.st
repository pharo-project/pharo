testing
testPrimitiveNumber
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPrimitive.
	self assert: (aRBMethode compiledMethod primitive = 4)