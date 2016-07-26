mathematical functions
raisedToInteger: anInteger 
	"See Number | raisedToInteger:"
	anInteger = 0 ifTrue: [^ 1].
	anInteger < 0 ifTrue: [^ self reciprocal raisedToInteger: anInteger negated].
	^ Fraction numerator: (numerator raisedToInteger: anInteger)
		denominator: (denominator raisedToInteger: anInteger)