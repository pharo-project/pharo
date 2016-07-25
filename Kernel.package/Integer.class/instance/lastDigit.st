system primitives
lastDigit
	"Answer the last digit of the integer base 256.  LargePositiveInteger uses bytes of base two number, and each is a 'digit'."

	^self digitAt: self digitLength