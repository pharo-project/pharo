testing
testPushNewArray

	| iRMethod aCompiledMethod receiver |
	iRMethod := IRBuilder new

		pushNewArray: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	
	receiver :=  (5@8).

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: receiver arguments: #()) first isNil).
	^iRMethod
