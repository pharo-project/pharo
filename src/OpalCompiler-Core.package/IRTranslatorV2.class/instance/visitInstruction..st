visiting
visitInstruction: instr
	gen mapBytesTo: instr.
	self visitNode: instr.
