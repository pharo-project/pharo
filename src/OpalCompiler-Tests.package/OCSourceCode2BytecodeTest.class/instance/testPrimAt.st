tests
testPrimAt
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimAt self at: 3') selector.
	scanner := InstructionStream on: method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#at:. false. 1}])
		description: 'Failed ' , selector