bit access
nextSingleBits: n
	| out |
	out := 0.
	1 to: n do:[:i| out := (out bitShift: 1) + (self nextBits: 1)].
	^out