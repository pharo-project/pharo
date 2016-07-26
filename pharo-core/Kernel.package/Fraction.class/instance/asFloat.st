converting
asFloat
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBitOfMagnitude.
	hb := b highBitOfMagnitude.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly in floating point number,
	then fastest thing to do is to use hardwired float division."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	First guess is rough, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will loose bits.
	Keep at least one bit to let underflow preserve the sign of zero."
	lostBit := Float emin - (exponent + hm - 1).
	lostBit > 0 ifTrue: [n := n - lostBit max: 1].

	"Remove excess bits in the mantissa."
	hm > n
		ifTrue:
			[exponent := exponent + hm - n.
			hasTruncatedBits := hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - n].
			mantissa := mantissa bitShift: n - hm].

	"Check if mantissa must be rounded upward.
	The case of tie (mantissa odd & hasTruncatedBits not)
	will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	^ (self positive
			ifTrue: [mantissa asFloat]
			ifFalse: [mantissa asFloat negated])
		timesTwoPower: exponent