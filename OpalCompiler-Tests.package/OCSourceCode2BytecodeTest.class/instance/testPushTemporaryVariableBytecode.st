tests
testPushTemporaryVariableBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushTemporaryVariableBytecode: t0 t0 yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushTemporaryVariable:
				and: [did arguments first == 0])
		description: 'Failed ' , selector