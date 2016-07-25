tests
testPushLiteralConstantBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushLiteralConstantBytecode #() yourself') selector.
	scanner := InstructionStream on: method.
	self assert: ((did := scanner peekInstruction) selector == #pushConstant:
					and: [did arguments first = #()])
		description: 'Failed ' , selector