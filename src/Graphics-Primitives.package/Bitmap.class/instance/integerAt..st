accessing
integerAt: index 
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 1073741823 ifTrue: [ ^ word ].	"Avoid LargeInteger computations"
	^ word >= 2147483648 
		ifTrue: 
			[ "Negative?!"
			"word - 16r100000000"
			(word bitInvert32 + 1) negated ]
		ifFalse: [ word ]