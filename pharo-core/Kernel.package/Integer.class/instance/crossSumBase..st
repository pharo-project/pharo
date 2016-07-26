arithmetic
crossSumBase: aBase
	|aResult|
	"Precondition"
	[aBase isInteger and: [aBase >=2]] assert.

	self < 0 ifTrue: [^self negated crossSumBase: aBase].
	self < aBase ifTrue: [^ self].
	aResult := self \\ aBase + (self // aBase crossSumBase: aBase).

	"Postcondition
	E.g. 18 crossSumBase: 10 -> 9 => 18\\(10-1) = 0"
	[((aResult \\ (aBase - 1) = 0)) = ((self \\ (aBase - 1)) =0)] assert.
	^aResult