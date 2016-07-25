testing
testInstVar

	| iRMethod aCompiledMethod  |
	iRMethod := IRBuilder new
		pushInstVar: 1;
		pushInstVar: 2;
		send: #+;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: (3@4) arguments: #() ) = 7).
	^iRMethod
	
	