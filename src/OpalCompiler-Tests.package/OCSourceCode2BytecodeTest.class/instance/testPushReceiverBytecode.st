tests
testPushReceiverBytecode
	| selector method scanner |
	selector := (method := self compile2method: 'pushReceiverBytecode ^ self yourself') selector.
	scanner := InstructionStream on: method.
	self assert: scanner peekInstruction selector == #pushReceiver
		description: 'Failed ' , selector