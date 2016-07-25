tests
testReturnReceiver
	| selector method scanner |
	selector := (method := self compile2method: 'returnReceiver 3 + 4. ^ self') selector.
	scanner := InstructionStream on: method.
	4
		timesRepeat: [scanner nextInstruction].
	self assert: scanner peekInstruction selector == #methodReturnReceiver
		description: 'Failed ' , selector