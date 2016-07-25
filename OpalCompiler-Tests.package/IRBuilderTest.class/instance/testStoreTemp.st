testing
testStoreTemp

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		addTemps: #(a);
		pushLiteral: 34;
		storeTemp: #a;
		popTop;
		pushTemp: #a;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 34).
	^iRMethod
	