debugging
fullBlockInstructionForPC: aPC
	| pc |
	"For a given PC, the actual instruction may start N bytes ahead."
	pc := aPC.
	[ pc >= 0 ] whileTrue: [ 
		(self firstInstructionMatching: [:ir | ir bytecodeOffset = pc ]) ifNotNil: [:it |^it].
		pc := pc - 1 ].