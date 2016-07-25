testing
testJumpAheadToIf

	| iRMethod aCompiledMethod |
	
	iRMethod := IRBuilder new
		pushReceiver;
		pushLiteral: true;
		jumpAheadTo: #end if: true;
		returnTop;
		jumpAheadTarget: #end;	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) isNil).
	^iRMethod.
	