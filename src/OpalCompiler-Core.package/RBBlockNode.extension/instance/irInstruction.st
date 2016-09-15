*OpalCompiler-Core
irInstruction
	^ self parent methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]