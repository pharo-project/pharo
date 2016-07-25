tests
testSuperSend
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodeSuperSend super yourself') selector.
	scanner := InstructionStream on: method.
	1
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#yourself. true. 0}])
		description: 'Failed ' , selector