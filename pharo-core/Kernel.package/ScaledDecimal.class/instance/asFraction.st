converting
asFraction	
	"Convert the receiver to a Fraction.
	Avoid using numerator / denominator to save a useless and costly gcd: computation"

	^denominator = 1
		ifTrue: [numerator]
		ifFalse: [Fraction numerator: numerator denominator: denominator]