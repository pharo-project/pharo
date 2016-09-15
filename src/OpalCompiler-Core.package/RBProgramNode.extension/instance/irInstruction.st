*opalcompiler-core
irInstruction
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]