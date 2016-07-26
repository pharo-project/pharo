arithmetic
- aNumber
	"Refer to the comment in Number - "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ self digitSubtract: aNumber]
			ifFalse: [^ (self digitAdd: aNumber) normalize]].
	aNumber isFraction ifTrue:
		[^Fraction numerator: self * aNumber denominator - aNumber numerator denominator: aNumber denominator].
	^ aNumber adaptToInteger: self andSend: #-