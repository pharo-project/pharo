mathematical functions
nthRootTruncated: aPositiveInteger
	"Answer the integer part of the nth root of the receiver."
	| guess guessToTheNthMinusOne delta |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootTruncated: aPositiveInteger) negated].
	guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
	[
		guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
		delta := (guess * guessToTheNthMinusOne - self) // (guessToTheNthMinusOne * aPositiveInteger).
		delta = 0 ] whileFalse:
			[ guess := guess - delta ].
	( (guess := guess - 1) raisedTo: aPositiveInteger) > self  ifTrue:
			[ guess := guess - 1 ].
	^guess