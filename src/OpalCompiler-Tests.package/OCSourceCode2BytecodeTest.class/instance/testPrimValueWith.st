tests
testPrimValueWith
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodeValueWith self value: 3') selector.
	scanner := InstructionStream on: method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#value:. false. 1}])
		description: 'Failed ' , selector