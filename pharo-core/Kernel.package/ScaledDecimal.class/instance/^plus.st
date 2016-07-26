arithmetic
+ aNumber
	aNumber class = self class ifTrue: [^self asFraction + aNumber asFraction asScaledDecimal: (scale max: aNumber scale)].
	^self coerce: self asFraction + aNumber