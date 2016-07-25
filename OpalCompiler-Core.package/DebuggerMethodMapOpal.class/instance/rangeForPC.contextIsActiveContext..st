public
rangeForPC: aPC contextIsActiveContext: contextIsActive
	"return the debug highlight for aPC"
	| pc |
	
	"When on the top of the stack the pc is pointing to right instruction, but deeper in the stack
	the pc was already advanced one bytecode, so we need to go back this one bytecode, which
	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of the
	IR instruction, which means we can just go back one"

	pc := contextIsActive ifTrue: [aPC] ifFalse: [aPC - 1].
	^(methodNode sourceNodeForPC:  pc) debugHighlightRange
	