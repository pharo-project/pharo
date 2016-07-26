mathematical functions
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger odd
				ifTrue: [ (self negated raisedTo: 1.0 / aPositiveInteger) negated ]
				ifFalse: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]