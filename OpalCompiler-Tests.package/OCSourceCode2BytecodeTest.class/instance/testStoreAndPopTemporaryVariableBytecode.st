tests
testStoreAndPopTemporaryVariableBytecode
	| selector method scanner did |
	selector := (method:= self compile2method: 'storeAndPopTemporaryVariableBytecode | t0 t1 | t1 := t0') selector.
	scanner := InstructionStream on: method.
	self
		assert: ((did := scanner nextInstruction; peekInstruction) selector == #popIntoTemporaryVariable:
				and: [did arguments first == 1])
		description: 'Failed ' , selector