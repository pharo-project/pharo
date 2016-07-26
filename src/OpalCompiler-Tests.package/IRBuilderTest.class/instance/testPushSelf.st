testing
testPushSelf

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushReceiver;
		send: #class;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) == UndefinedObject).
	^iRMethod
	