tests
testStoreIntoLiteralVariableBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'storeIntoLiteralVariableBytecode ^ (ClassVar := nil)') selector.
	scanner := InstructionStream on: method.
	self
		assert: (did := scanner nextInstruction; peekInstruction) selector == #storeIntoLiteralVariable:
		description: 'Failed ' , selector