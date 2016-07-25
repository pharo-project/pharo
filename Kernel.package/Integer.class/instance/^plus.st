arithmetic
+ aNumber
	"Refer to the comment in Number + "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ (self digitAdd: aNumber) normalize]
			ifFalse: [^ self digitSubtract: aNumber]].
	aNumber isFraction ifTrue:
		[^Fraction numerator: self * aNumber denominator + aNumber numerator denominator: aNumber denominator].
	^ aNumber adaptToInteger: self andSend: #+