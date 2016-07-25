tests
testPrimSize
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimSize self size') selector.
	scanner := InstructionStream on: method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#size. false. 0}])
		description: 'Failed ' , selector