adding
addInstructions: aCollection before: anInstruction

	aCollection do: [:instr | self add: instr before: anInstruction].