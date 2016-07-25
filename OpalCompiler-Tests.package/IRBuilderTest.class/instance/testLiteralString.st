testing
testLiteralString

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: 'hello'; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 'hello').
	^iRMethod

	