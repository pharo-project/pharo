character classification
isDigit: char
	"Answer whether the receiver is a digit."

	| value |
	value := char asciiValue.
	^ value >= 48 and: [value <= 57].
