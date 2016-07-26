tests
testPushConstantTrueBytecode
	| selector method scanner did |
	selector := (method := self compile2method: 'pushConstantTrueBytecode true yourself') selector.
	scanner := InstructionStream on: method.
	self assert: ((did := scanner peekInstruction) selector == #pushConstant:
					and: [did arguments first])
		description: 'Failed ' , selector