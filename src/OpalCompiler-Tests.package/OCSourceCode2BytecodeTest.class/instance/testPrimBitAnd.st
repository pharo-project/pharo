tests
testPrimBitAnd
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimBitAnd 3 bitAnd: 4') selector.
	scanner := InstructionStream on: method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#bitAnd:. false. 1}])
		description: 'Failed ' , selector