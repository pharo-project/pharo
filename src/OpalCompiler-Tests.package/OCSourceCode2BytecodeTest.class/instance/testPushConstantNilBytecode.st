tests
testPushConstantNilBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantNilBytecode nil yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushConstant:
				and: [did arguments first isNil])
		description: 'Failed ' , selector