instruction stream support
interpretNextInstructionFor: aClient in: anInstructionStream
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^ anInstructionStream interpretNextV3PlusClosureInstructionFor: aClient