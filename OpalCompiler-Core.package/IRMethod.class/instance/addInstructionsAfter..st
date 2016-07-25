inlining
addInstructionsAfter: aCollection
	| returningSeqs  lastInstr |
	aCollection ifEmpty: [^self].
	returningSeqs := self allSequences select: [:each | each last isReturn].
	lastInstr := returningSeqs last last.
	lastInstr addInstructionsBefore: aCollection.
	