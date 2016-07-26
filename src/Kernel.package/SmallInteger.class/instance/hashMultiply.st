bit manipulation
hashMultiply
	| low |

	low := self bitAnd: 16383.
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))
			bitAnd: 16r0FFFFFFF