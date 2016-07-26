private
montgomeryNumberOfDigits
	"Answer the number of montgomery digits required to represent the receiver."
	^self digitLength * 8 + (self montgomeryDigitLength - 1) // self montgomeryDigitLength