tests
testPrimBitOr
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimBitOr 3 bitOr: 4') selector.
	scanner := InstructionStream on:  method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#bitOr:. false. 1}])
		description: 'Failed ' , selector