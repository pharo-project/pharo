testing - adding
testAddIntructionsBefore
	
	| iRMethod aCompiledMethod push |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.

	push := (iRMethod allSequences last) first.

	(iRMethod allSequences last)
			addInstructions: {(IRInstruction pushLiteral: 2). (IRInstruction returnTop)} before: push.

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
