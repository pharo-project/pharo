tests
testSuperSendWith
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodeSuperSendWith super new: 0') selector.
	scanner := InstructionStream on: method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#new:. true. 1}])
		description: 'Failed ' , selector