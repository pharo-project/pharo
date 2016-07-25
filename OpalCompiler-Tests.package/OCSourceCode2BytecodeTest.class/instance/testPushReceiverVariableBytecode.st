tests
testPushReceiverVariableBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushReceiverVariableBytecode instVar yourself') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner peekInstruction) selector == #pushReceiverVariable:
				and: [did arguments first == 2])
		description: 'Failed ' , selector