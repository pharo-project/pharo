testing
testLiteralArray

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: #(test 4 you); 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = #(test 4 you)).
	^iRMethod

	