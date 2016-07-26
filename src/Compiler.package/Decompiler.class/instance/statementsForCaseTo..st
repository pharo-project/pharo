control
statementsForCaseTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a CaseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for CaseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == CaseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos