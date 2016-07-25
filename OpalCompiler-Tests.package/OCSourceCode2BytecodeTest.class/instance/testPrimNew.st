tests
testPrimNew
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodePrimNew super new') selector.
	scanner := InstructionStream on:  method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#new. true. 0}])
		description: 'Failed ' , selector