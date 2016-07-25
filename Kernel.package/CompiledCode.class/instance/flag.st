accessing
flag
	"Answer the user-level flag bit"

	^((self header bitShift: -29) bitAnd: 1) = 1