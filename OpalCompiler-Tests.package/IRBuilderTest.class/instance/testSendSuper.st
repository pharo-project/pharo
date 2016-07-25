testing
testSendSuper

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushReceiver;
		send: #isThisEverCalled toSuperOf: self class;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self should: [(aCompiledMethod valueWithReceiver: (self class new) arguments: #())] raise: Error.
	^iRMethod
	