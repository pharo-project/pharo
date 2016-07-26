printing
numberOfDigitsInBase: b 
	"Return how many digits are necessary to print this number in base b.
	Mostly same as super but an optimized version for base 10 case"
	
	b = 10 ifFalse: [^super numberOfDigitsInBase: b].
	self < 0 ifTrue: [^self negated numberOfDigitsInBase: b].
	^self decimalDigitLength