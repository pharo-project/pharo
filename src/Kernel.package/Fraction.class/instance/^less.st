comparing
< aNumber
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator < (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andCompare: #<