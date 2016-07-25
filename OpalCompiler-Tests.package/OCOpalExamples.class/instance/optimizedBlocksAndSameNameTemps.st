examples
optimizedBlocksAndSameNameTemps
	| s c |
	s := self isNil
			ifTrue: [| a | a := 1. a]
			ifFalse: [| a | a := 2. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c