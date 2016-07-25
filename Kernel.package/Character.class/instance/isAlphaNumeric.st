testing
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."

	^self isLetter or: [self isDigit]