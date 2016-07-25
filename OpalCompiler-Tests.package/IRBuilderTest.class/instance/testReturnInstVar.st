testing
testReturnInstVar

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushInstVar: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: (3@4) arguments: #() ) = 3).
	^iRMethod
	
	