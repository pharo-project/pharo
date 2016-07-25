testing
testReturnTop

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteral: false;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self deny: (aCompiledMethod valueWithReceiver: nil arguments: #()).
	^iRMethod
	