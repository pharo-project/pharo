tests
testReturnTopFromMethod
	| selector method scanner |
	selector := (method := self compile2method: 'returnTopFromMethod ^ 3 + 4') selector.
	scanner := InstructionStream on: method.
	3
		timesRepeat: [scanner nextInstruction].
	self assert: scanner peekInstruction selector == #methodReturnTop
		description: 'Failed ' , selector