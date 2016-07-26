testing
testJumpBackTo

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushReceiver;
		pushLiteral: false;
		jumpBackTarget: #begin;
		jumpAheadTo: #end if: true;
		pushLiteral: true;
		jumpBackTo: #begin;
		jumpAheadTarget: #end;	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) isNil).
	^iRMethod.
	