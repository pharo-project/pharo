mathematical functions
nthRootRounded: aPositiveInteger
	"Answer the integer nearest the nth root of the receiver."
	| guess |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue:
			[aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootRounded: aPositiveInteger) negated].
	guess := self nthRootTruncated: aPositiveInteger.
	^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
		ifTrue: [guess + 1]
		ifFalse: [guess]