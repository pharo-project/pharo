accessing
clearFlag
	"Clear the user-level flag bit"

	self objectAt: 1 put: (self header bitAnd: (1 << 29) bitInvert)