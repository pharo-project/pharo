instruction decoding
jump: offset if: condition 
	"Conditional Jump bytecode."
	stackPointer := stackPointer - 1.
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer]