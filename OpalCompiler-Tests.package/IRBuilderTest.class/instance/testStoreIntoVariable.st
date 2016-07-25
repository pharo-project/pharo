testing
testStoreIntoVariable

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushLiteral: 4;
		storeIntoLiteralVariable: (self class bindingOf: #TestToPush);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
      aCompiledMethod valueWithReceiver: nil arguments: #().
	self assert: (self class testToPush = 4).
	self class testToPush: nil.
	^iRMethod
	