instruction decoding
interpret
	| endPC |
	endPC := instructionStream method endPC.
	[instructionStream pc > endPC ] 
		whileFalse: [
			self checkIfJumpTarget.
			irBuilder mapToByteIndex: instructionStream pc.
			instructionStream interpretNextInstructionFor: self.]