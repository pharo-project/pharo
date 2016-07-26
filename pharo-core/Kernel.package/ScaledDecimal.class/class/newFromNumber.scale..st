instance creation
newFromNumber: aNumber scale: anInteger
	| aFraction |
	aFraction := aNumber asFraction.
	^aFraction isFraction
		ifTrue: [self new setNumerator: aFraction numerator denominator: aFraction denominator scale: anInteger]
		ifFalse: [self new setNumerator: aFraction denominator: 1 scale: anInteger]