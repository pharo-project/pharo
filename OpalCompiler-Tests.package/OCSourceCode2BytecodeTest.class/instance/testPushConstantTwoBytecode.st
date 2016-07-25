tests
testPushConstantTwoBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantTwoBytecode 2 yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushConstant:
				and: [did arguments first == 2])
		description: 'Failed ' , selector