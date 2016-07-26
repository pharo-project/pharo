testing - adding
testAddIntructions
	
	| iRMethod aCompiledMethod |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.

	(iRMethod allSequences last) last delete.
	(iRMethod allSequences last) last delete.

	(iRMethod allSequences last)
			addInstructions: {(IRInstruction pushLiteral: 2). (IRInstruction returnTop)}.

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
