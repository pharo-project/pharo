examples-blocks-optimized
exampleMethodWithOptimizedBlocksA
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c. "notNil"