mathematical functions
log
	"This function is defined because super log might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0' from: 0].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal log negated].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log