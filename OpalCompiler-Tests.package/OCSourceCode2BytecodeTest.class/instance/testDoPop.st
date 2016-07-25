tests
testDoPop
	| selector method scanner |
	selector := (method := self compile2method: 'popStackBytecode 3 + 4') selector.
	scanner := InstructionStream on:  method.
	3
		timesRepeat: [scanner nextInstruction].
	self assert: scanner peekInstruction selector == #doPop description: 'Failed ' , selector