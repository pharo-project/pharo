tests
testPushConstantMinusOneBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantMinusOneBytecode -1 yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushConstant:
				and: [did arguments first == -1])
		description: 'Failed ' , selector