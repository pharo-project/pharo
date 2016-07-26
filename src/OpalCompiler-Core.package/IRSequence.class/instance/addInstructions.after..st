adding
addInstructions: aCollection after: anInstruction
	
	^aCollection reverseDo: [:instr | self add: instr after: anInstruction].