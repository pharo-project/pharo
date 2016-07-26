testing
testPushTempTemp

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		addTemps: #(a);
		pushTemp: #a;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: 5 arguments: #() ) isNil).
	^iRMethod
	