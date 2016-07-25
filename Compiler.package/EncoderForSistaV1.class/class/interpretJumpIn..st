instruction stream support
interpretJumpIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct unconditional jump decoder for the instruction set."
	^anInstructionStream interpretSistaV1Jump