tests
testReturnTrueBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'returnTrueBytecode 3 + 4. ^ true') selector.
	scanner := InstructionStream on: method.
	4
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #methodReturnConstant:
				and: [did arguments first])
		description: 'Failed ' , selector