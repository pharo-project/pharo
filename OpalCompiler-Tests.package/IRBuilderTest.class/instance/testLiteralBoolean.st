testing
testLiteralBoolean

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: true; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() )).
	^iRMethod.

	