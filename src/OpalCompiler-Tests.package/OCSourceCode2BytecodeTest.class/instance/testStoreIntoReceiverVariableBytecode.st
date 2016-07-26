tests
testStoreIntoReceiverVariableBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'storeIntoReceiverVariableBytecode ^ (instVar := self)') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner nextInstruction; peekInstruction) selector == #storeIntoReceiverVariable:
				and: [did arguments first == 2])
		description: 'Failed ' , selector