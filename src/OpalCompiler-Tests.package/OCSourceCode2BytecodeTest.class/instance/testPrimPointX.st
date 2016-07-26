tests
testPrimPointX
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimPointX self x') selector.
	scanner := InstructionStream on: method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#x. false. 0}])
		description: 'Failed ' , selector