converting
asIEEE32BitWord
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in FloatArrays only."
	
	| word1 word2 sign mantissa exponent destWord truncatedBits mask roundToUpper |
	
	"skip fast positive and nnegative zero"
	self = 0.0 ifTrue: [^self basicAt: 1].
	
	"retrieve 64 bits of IEEE 754 double"
	word1 := self basicAt: 1.
	word2 := self basicAt: 2.
	
	"prepare sign exponent and mantissa of 32 bits float"
	sign := word1 bitAnd: 16r80000000.
	exponent := ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	mantissa := (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	truncatedBits := (word2 bitAnd: 16r1FFFFFFF).

	"We must now honour default IEEE rounding mode (round to nearest even)"
	
	"we are below gradual underflow, even if rounded to upper mantissa"
	exponent < -24 ifTrue: [^sign "this can be negative zero"].
	
	"BEWARE: rounding occurs on less than 23bits when gradual underflow"
	exponent <= 0
		ifTrue:
			[mask := 1 bitShift: exponent negated.
			mantissa := mantissa bitOr: 16r800000.
			roundToUpper := (mantissa bitAnd: mask) isZero not
				and: [truncatedBits isZero not
					or: [(mantissa bitAnd: mask - 1) isZero not
						or: [(mantissa bitAnd: mask*2) isZero not]]].
			mantissa := mantissa bitShift: exponent - 1.
			"exponent := exponent + 1"]
		ifFalse:
			[roundToUpper := (truncatedBits bitAnd: 16r10000000) isZero not
				and: [(mantissa bitAnd: 16r1) isZero not
					or: [(truncatedBits bitAnd: 16r0FFFFFFF) isZero not]]
			].
		
	"adjust mantissa and exponent due to IEEE rounding mode"
	roundToUpper
		ifTrue:
			[mantissa := mantissa + 1.
			mantissa > 16r7FFFFF
				ifTrue:
					[mantissa := 0.
					exponent := exponent+1]].

	exponent > 254 ifTrue: ["Overflow"
		exponent := 255.
		self isNaN
			ifTrue: [mantissa isZero
				ifTrue: ["BEWARE: do not convert a NaN to infinity due to truncatedBits"
					mantissa := 1]]
			ifFalse: [mantissa := 0]].
		
	"Encode the word"
	destWord := (sign bitOr: ((exponent max: 0) bitShift: 23)) bitOr: mantissa.
	^ destWord