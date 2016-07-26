tests
testReturnTopFromBlock
	| selector method scanner |
	
	selector := (method := self compile2method: 'returnTopFromBlock |ia| ^[ia]') selector.
	scanner := InstructionStream on: method.
	3 timesRepeat: [scanner nextInstruction].
	self assert: scanner peekInstruction selector == #blockReturnTop
		description: ('Failed ' , selector)