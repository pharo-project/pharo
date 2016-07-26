accessing
endsWithDigit
	"Answer whether the receiver's final character represents a digit."

	^ self size > 0 and: [self last isDigit]