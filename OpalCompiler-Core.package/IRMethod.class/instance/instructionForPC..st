debugging
instructionForPC: aPC
	| initialPC pc |
	"generates the compiledMethod and optimize the ir. 
	Removes the side-effect of optimizing the IR while looking for instruction, 
	which results in incorrect found instruction"
	initialPC := self compiledMethod initialPC.
	"For a given PC, the actual instruction may start N bytes ahead."
	pc := aPC.
	[ pc >= initialPC ] whileTrue: [ 
		(self firstInstructionMatching: [:ir | ir bytecodeOffset = pc ]) ifNotNil: [:it |^it].
		pc := pc - 1 ].
	^self "if we not found anything then this method is our target instruction"