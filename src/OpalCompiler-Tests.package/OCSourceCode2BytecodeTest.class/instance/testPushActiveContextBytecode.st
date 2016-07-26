tests
testPushActiveContextBytecode
	| selector method scanner |
	selector := (method := self compile2method: 'pushActiveContextBytecode thisContext yourself') selector.
	scanner := InstructionStream on: method.
	self assert: scanner peekInstruction selector == #pushActiveContext
		description: 'Failed ' , selector