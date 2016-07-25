code examples
methodWithOptimizedBlocks
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| b | b := 'notNil'. b].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocks)
		class: self class"