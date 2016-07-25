arithmetic
- aNumber
	"Answer the difference between the receiver and aNumber."
	aNumber isInteger ifTrue:
		[^Fraction numerator: numerator - (denominator * aNumber) denominator: denominator].
	aNumber isFraction ifTrue:
		[^ self + aNumber negated].
	^ aNumber adaptToFraction: self andSend: #-