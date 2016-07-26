character classification
isLowercase: char
	"Answer whether the receiver is a lowercase letter.
	(The old implementation answered whether the receiver is not an uppercase letter.)"

	| value |
	value := char asciiValue.
	^ 8r141 <= value and: [value <= 8r172].
