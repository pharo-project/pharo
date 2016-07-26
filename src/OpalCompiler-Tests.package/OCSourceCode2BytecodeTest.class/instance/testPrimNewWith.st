tests
testPrimNewWith
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodeNewWith self new: 3') selector.
	scanner := InstructionStream on:  method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#new:. false. 1}])
		description: 'Failed ' , selector