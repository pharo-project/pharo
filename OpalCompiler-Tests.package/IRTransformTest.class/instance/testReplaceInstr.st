testing - adding
testReplaceInstr
	
	| iRMethod aCompiledMethod |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.
	
	(iRMethod allSequences last first) 
			replaceWithInstructions: {(IRInstruction pushLiteral: 2)}.

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
