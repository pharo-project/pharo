accessing
size
	| n cell |
	n := 0.
	cell := self.
	[cell isNil] whileFalse: 
			[n := n + 1.
			cell := cell nextCell].
	^n