code generation
emitCodeForEvaluatedEffect: stack encoder: encoder
	| position |
	position := stack position.
	self returns
		ifTrue: 
			[self emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1]
		ifFalse: 
			[self emitCodeExceptLast: stack encoder: encoder.
			statements last emitCodeForEffect: stack encoder: encoder].
	[stack position = position] assert