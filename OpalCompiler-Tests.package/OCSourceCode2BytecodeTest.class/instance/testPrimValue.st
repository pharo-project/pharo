tests
testPrimValue
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimValue self value') selector.
	scanner := InstructionStream on:  method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#value. false. 0}])
		description: 'Failed ' , selector