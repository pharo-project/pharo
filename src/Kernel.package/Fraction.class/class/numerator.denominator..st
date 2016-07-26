instance creation
numerator: numInteger denominator: denInteger 
	"Answer an instance of me (numInteger/denInteger).
	NOTE: This primitive initialization method will not reduce improper fractions,
	so normal usage should be coded as, eg,
		(Fraction numerator: a denominator: b) reduced
	or, more simply, as
		a / b."

	^self new setNumerator: numInteger denominator: denInteger