testing - adding
testAdd
	
	| iRMethod aCompiledMethod |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.

	(iRMethod allSequences last) last delete.
	(iRMethod allSequences last) last delete.

	(iRMethod allSequences last)
			add: (IRInstruction pushLiteral: 2).

	(iRMethod allSequences last)
			add: (IRInstruction returnTop).

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
