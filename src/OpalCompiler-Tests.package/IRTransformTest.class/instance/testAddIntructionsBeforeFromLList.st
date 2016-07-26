testing - adding
testAddIntructionsBeforeFromLList
	
	| iRMethod aCompiledMethod push llist col |

	iRMethod := IRBuilder new
		pushLiteral: 1;				
		returnTop;
		ir.

	push := (iRMethod allSequences last) at: 1 .

	llist := LinkedList new.
	llist add: (IRInstruction pushLiteral: 2).
	llist add: (IRInstruction returnTop).

	col := llist asOrderedCollection.

	(iRMethod allSequences last)
			addInstructions:  col before: push.

	aCompiledMethod := iRMethod compiledMethod.
	self should: [(aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2].
