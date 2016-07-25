character classification
isUppercase: char
	"Answer whether the receiver is an uppercase letter.
	(The old implementation answered whether the receiver is not a lowercase letter.)"

	| value |
	value := char asciiValue.
	^ 8r101 <= value and: [value <= 8r132].
