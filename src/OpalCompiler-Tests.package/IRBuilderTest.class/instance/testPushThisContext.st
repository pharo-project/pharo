testing
testPushThisContext

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushThisContext;
		send: #receiver;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: 5 arguments: #() ) = 5).
	^iRMethod
	