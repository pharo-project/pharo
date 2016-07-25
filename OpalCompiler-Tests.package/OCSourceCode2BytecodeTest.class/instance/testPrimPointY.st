tests
testPrimPointY
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimPointY self y') selector.
	scanner := InstructionStream on: method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#y. false. 0}])
		description: 'Failed ' , selector