tests
testPrimAtEnd
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimSize self atEnd') selector.
	scanner := InstructionStream on:  method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#atEnd. false. 0}])
		description: 'Failed ' , selector