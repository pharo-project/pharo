accessing
at: index
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]