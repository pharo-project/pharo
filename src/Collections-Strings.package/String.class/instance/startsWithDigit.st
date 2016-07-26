accessing
startsWithDigit
	"Answer whether the receiver's first character represents a digit"

	^ self size > 0 and: [self first isDigit]