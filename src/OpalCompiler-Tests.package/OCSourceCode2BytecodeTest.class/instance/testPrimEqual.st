tests
testPrimEqual
	| selector method scanner did |
	selector := (method  := self compile2method: 'bytecodePrimEqual 3 = 4') selector.
	scanner := InstructionStream on:  method.
	2
		timesRepeat: [scanner nextInstruction].
	self
		assert: ((did := scanner peekInstruction) selector == #send:super:numArgs:
				and: [did arguments = {#'='. false. 1}])
		description: 'Failed ' , selector