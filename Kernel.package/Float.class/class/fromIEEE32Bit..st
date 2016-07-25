instance creation
fromIEEE32Bit: word
	"Convert the given 32 bit word (which is supposed to be a positive 32-bit value) from
	 a 32 bit IEEE floating point representation into an actual Squeak float object (being
	 64 bits wide). Should only be used for conversion in FloatArrays or likewise objects."
	
	| sign mantissa exponent delta |
	word <= 0 ifTrue:
		[^word negative
			ifTrue: [self error: 'Cannot deal with negative numbers']
			ifFalse: [self zero]].
	sign := word bitAnd: 16r80000000.
	word = sign ifTrue:
		[^self negativeZero].
	
	exponent := ((word bitShift: -23) bitAnd: 16rFF) - 127.
	mantissa := word bitAnd:  16r7FFFFF.

	exponent = 128 ifTrue: "Either NAN or INF"
		[^mantissa = 0
			ifTrue:
				[sign = 0 
					ifTrue: [self infinity]
					ifFalse: [self negativeInfinity]]
			ifFalse: [self nan]].

	exponent = -127 ifTrue:
		"gradual underflow (denormalized number)
		 Remove first bit of mantissa and adjust exponent"
		[delta := mantissa highBit.
		 mantissa := (mantissa bitAnd: (1 bitShift: delta - 1) - 1) bitShift: 24 - delta.
		 exponent := exponent + delta - 23].
	
	"Create new float"
	^(self basicNew: 2)
		basicAt: 1 put: ((sign bitOr: (1023 + exponent bitShift: 20)) bitOr: (mantissa bitShift: -3));
		basicAt: 2 put: ((mantissa bitAnd: 7) bitShift: 29);
		* 1.0 "reduce to SmallFloat64 if possible"