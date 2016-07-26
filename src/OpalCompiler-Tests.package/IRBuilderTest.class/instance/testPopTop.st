testing
testPopTop

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushReceiver;
		pushLiteral: false;
		popTop;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) isNil).
	^iRMethod
	