truncation and round off
ulp
	"Answer the unit of least precision of self (the power of two corresponding to last bit of mantissa)"
	
	| exponent |
	self isFinite ifFalse: [
		self isNaN ifTrue: [^self].
		^Float infinity].
	self = 0.0 ifTrue: [^Float fmin].
	exponent := self exponent.
	^exponent < self class emin
		ifTrue: [Float fminDenormalized]
 		ifFalse: [Float epsilon timesTwoPower: exponent]