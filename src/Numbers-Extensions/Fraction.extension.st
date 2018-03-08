Extension { #name : #Fraction }

{ #category : #'*Numbers-Extensions' }
Fraction >> ln [
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln
]

{ #category : #'*Numbers-Extensions' }
Fraction >> log [
	"This function is defined because super log might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal log negated].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log
]

{ #category : #'*Numbers-Extensions' }
Fraction >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	| guess |
	guess := (numerator nthRootTruncated: aPositiveInteger) / (denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [^guess].
	"There is no exact nth root, so answer a Float approximation"
	^(self abs ln / aPositiveInteger) exp * self sign
]

{ #category : #'*Numbers-Extensions' }
Fraction >> raisedToFraction: aFraction [
	| root |
	root := (self numerator nthRootTruncated: aFraction denominator) / (self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [^root raisedToInteger: aFraction numerator].
	^super raisedToFraction: aFraction
]

{ #category : #'*Numbers-Extensions' }
Fraction >> raisedToInteger: anInteger [ 
	"See Number | raisedToInteger:"
	anInteger = 0 ifTrue: [^ 1].
	anInteger < 0 ifTrue: [^ self reciprocal raisedToInteger: anInteger negated].
	^ Fraction numerator: (numerator raisedToInteger: anInteger)
		denominator: (denominator raisedToInteger: anInteger)
]

{ #category : #'*Numbers-Extensions' }
Fraction >> sqrt [
	| d n |
	n := numerator sqrt.
	d := denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	^n / d
]

{ #category : #'*Numbers-Extensions' }
Fraction >> squared [
	"See Fraction (Number) | squared"
	^ Fraction numerator: numerator squared denominator: denominator squared
]
