testing - adding
testDelete
	
	| iRMethod aCompiledMethod |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		pushLiteral: 2;	
		returnTop;
		ir.

	((iRMethod allSequences last) 
		detect: [:each | each isPushLiteral: [:c | c == 2]]) delete.
			

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 1].
