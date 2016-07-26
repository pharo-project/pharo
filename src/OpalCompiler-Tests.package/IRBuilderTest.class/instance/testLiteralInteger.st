testing
testLiteralInteger

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: 2; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2).
	^iRMethod

	