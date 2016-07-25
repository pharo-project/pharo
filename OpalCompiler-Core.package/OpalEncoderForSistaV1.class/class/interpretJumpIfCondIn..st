instruction stream support
interpretJumpIfCondIn: anInstructionStream
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretSistaV1JumpIfCond