instruction decoding
interpretMethod: method
	[ instructionStream pc > method endPC ]
		whileFalse: [ 
			currentPC := self pc.
			instructionStream interpretNextInstructionFor: self ]