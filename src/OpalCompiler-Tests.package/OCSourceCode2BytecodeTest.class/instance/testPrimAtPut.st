tests
testPrimAtPut
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimAtPut self at: 3 put: 4') selector.
	scanner := InstructionStream on:  method.
	3
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#at:put:. false. 2}])
		description: 'Failed ' , selector