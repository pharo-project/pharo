truncation and round off
significandAsInteger
	"Answer the mantissa of a Float shifted so as to have the ulp equal to 1.
	For exceptional values, infinity and nan, just answer the bit pattern."

	self isFinite ifTrue:
		[^(self timesTwoPower: self class precision - 1 - (self exponent max: self class emin)) truncated abs].
	^(((self basicAt: 1) bitAnd: 16r000FFFFF) bitShift: 32) bitOr: (self basicAt: 2)