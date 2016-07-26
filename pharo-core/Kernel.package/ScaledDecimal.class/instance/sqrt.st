mathematical functions
sqrt
	"Answer the square root of the receiver.
	Preserve receiver class and scale if answer is exact.
	Otherwise, answer a Float to denote inexactness."
	| squareRoot |
	squareRoot := self asFraction sqrt.
	^squareRoot isFloat
		ifTrue: [squareRoot]
		ifFalse: [squareRoot asScaledDecimal: scale]