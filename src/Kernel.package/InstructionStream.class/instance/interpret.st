decoding
interpret
	| endPC |
	endPC := self method endPC.
	[pc > endPC] whileFalse: [self interpretNextInstructionFor: self]