testing
testLiteralVariableGlobale

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteralVariable: (Smalltalk globals associationAt: #Smalltalk);	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = Smalltalk).
	^iRMethod

	