tests
testDoDup
	| selector method scanner |
	method := self compile2method: 'duplicateTopBytecode 3 + 4; yourself'.
	selector := method selector.
	scanner := InstructionStream on:  method.
	1
		timesRepeat: [scanner nextInstruction].
	self assert: scanner peekInstruction selector == #doDup description: 'Failed ' , selector