testing
testLiteralFloat

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: 2.0; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) closeTo: 2.0).
	^iRMethod

	