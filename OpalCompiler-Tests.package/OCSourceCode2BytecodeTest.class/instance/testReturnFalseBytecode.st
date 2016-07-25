tests
testReturnFalseBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'returnFalseBytecode 3 + 4. ^ false') selector.
	scanner := InstructionStream on: method.
	4
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #methodReturnConstant:
				and: [did arguments first not])
		description: 'Failed ' , selector