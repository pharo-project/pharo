testing - adding
testAddBefore
	
	| iRMethod aCompiledMethod ret |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.

	(iRMethod allSequences last) last delete.
	(iRMethod allSequences last) last delete.

	ret :=  (IRInstruction returnTop).

	(iRMethod allSequences last)
			add: ret.

	(iRMethod allSequences last)
			add: (IRInstruction pushLiteral: 2) before: ret.

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
