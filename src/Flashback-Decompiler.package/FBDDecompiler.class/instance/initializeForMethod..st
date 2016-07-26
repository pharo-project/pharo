initialize
initializeForMethod: aCompiledMethod
	loopsArray := FBDLoopScanner scan: aCompiledMethod .
	instructionStream := InstructionStream on: aCompiledMethod.
	self initializeStackForMethod: aCompiledMethod.
	jumpSize := 0