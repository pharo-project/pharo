testing
testStorePopIvar

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushLiteral: 34;
		storeInstVar: 2;
		popTop;
		pushInstVar: 2;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: self arguments: #() ) = 34).
	^iRMethod
	