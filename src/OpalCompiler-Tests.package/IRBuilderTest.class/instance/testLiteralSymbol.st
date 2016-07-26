testing
testLiteralSymbol

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: #you; 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = #you).
	^iRMethod

	