tests
testPrimDoWith
	| selector method scanner did |
	selector := (method := self compile2method: 'bytecodeDoWith self do: #something') selector.
	scanner := InstructionStream on:  method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#do:. false. 1}])
		description: 'Failed ' , selector