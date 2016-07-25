testing
testLiteralCharacter

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: $e; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = $e).
	^iRMethod

	