tests
testStoreAndPopReceiverVariableBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'storeAndPopReceiverVariableBytecode instVar := self') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner nextInstruction; peekInstruction) selector == #popIntoReceiverVariable:
				and: [did arguments first == 2])
		description: 'Failed ' , selector