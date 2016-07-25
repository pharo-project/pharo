testing
testLiteralNil

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: nil; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: 4 arguments: #() ) isNil).
	^iRMethod
	