tests
testPushConstantFalseBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantFalseBytecode false yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushConstant:
				and: [did arguments first not])
		description: 'Failed ' , selector