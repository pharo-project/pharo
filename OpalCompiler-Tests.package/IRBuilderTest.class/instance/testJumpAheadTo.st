testing
testJumpAheadTo

	| iRMethod aCompiledMethod iRBuilder |
	iRBuilder := IRBuilder new.
	iRMethod := iRBuilder
		pushLiteral: 2;
		pushLiteral: 1;
		send: #+;
		jumpAheadTo: #end;
		pushLiteral: 3;
		jumpAheadTarget: #end;	
		returnTop;
		ir.


	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil arguments: #())  = 3.
	^iRMethod
	