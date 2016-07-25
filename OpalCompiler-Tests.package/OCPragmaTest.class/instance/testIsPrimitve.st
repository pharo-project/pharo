testing
testIsPrimitve
	| aRBMethode |

	aRBMethode := OpalCompiler new parse: self methodPrimitive.
	self assert: aRBMethode isPrimitive