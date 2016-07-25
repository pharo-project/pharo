tests
testReturnNilBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'returnNilBytecode 3 + 4. ^ nil') selector.
	scanner := InstructionStream on: method.
	4
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #methodReturnConstant:
				and: [did arguments first isNil])
		description: 'Failed ' , selector