tests
testPrimNext
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimSize self next') selector.
	scanner := InstructionStream on:  method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#next. false. 0}])
		description: 'Failed ' , selector