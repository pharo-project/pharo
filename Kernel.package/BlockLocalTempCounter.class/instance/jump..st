instruction decoding
jump: offset
	"Unconditional Jump bytecode."
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer.
		 self doJoin]