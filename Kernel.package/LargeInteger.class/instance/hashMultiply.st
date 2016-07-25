bit manipulation
hashMultiply
	"Truncate to 28 bits and try again"

	^(self bitAnd: 16rFFFFFFF) hashMultiply