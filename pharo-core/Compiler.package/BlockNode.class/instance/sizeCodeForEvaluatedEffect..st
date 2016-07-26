code generation
sizeCodeForEvaluatedEffect: encoder

	^self returns
		ifTrue: [self sizeCodeForEvaluatedValue: encoder]
		ifFalse: [(self sizeCodeExceptLast: encoder)
				+ (statements last sizeCodeForEffect: encoder)]