tests
testPushConstantZeroBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantZeroBytecode 0 yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushConstant:
				and: [did arguments first == 0])
		description: 'Failed ' , selector