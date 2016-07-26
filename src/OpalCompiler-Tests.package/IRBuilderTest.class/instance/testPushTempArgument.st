testing
testPushTempArgument

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		numArgs: 2;
		addTemps: #(a b);
		pushTemp: #a;
		pushTemp: #b;
		send: #+;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #(2 8) ) = 10).
	^iRMethod
	