parsing-private
makeIntegerOrScaledIntegerOrFloat
	"at this point, there is no digit, nor fractionPart.
	maybe it can be a scaled decimal with fraction omitted...
	integer with negative exponent can be floats not fractions"
	
	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent ifTrue:
		["Check that the result is an integer, otherwise answer a Float.  Fractions are /not/ valid literals."
		 (exponent >= 0 or: [(integerPart * (base raisedToInteger: exponent)) isInteger]) ifFalse:
			[base := base asFloat].
		^integerPart * (base raisedToInteger: exponent)].
	(self readScaleWithDefaultNumberOfDigits: 0)
		ifTrue: [^integerPart asScaledDecimal: scale].
	^ integerPart