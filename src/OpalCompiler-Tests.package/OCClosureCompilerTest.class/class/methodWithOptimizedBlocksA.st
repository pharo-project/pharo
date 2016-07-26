code examples
methodWithOptimizedBlocksA
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocksA)
		class: self class"