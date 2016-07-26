converting
asScaledDecimal
	"Convert the receiver to a ScaledDecimal.
	If there is a finite decimal representation of the receiver, then use the exact number of decimal places required.
	Else, use a default number of decimals."
	
	| pow2 pow5 q q5 |
	pow2 := denominator lowBit - 1.
	q := denominator bitShift: pow2 negated.
	pow5 := 0.
	[q = 1]
		whileFalse: [
			q5 := q // 5.
			(q - (5 * q5)) = 0 ifFalse: [^super asScaledDecimal].
			q := q5.
			pow5 := pow5 + 1].
	^self asScaledDecimal: (pow2 max: pow5)