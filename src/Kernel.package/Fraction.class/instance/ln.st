mathematical functions
ln
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln