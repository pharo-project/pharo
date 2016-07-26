testing
testLiteralVariableClass

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteralVariable: Object binding;	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = Object).
	^iRMethod

	