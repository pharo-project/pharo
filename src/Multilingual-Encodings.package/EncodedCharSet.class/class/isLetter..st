character classification
isLetter: char
	"Answer whether the receiver is a letter."

	| value |
	value := char asciiValue.
	^ (8r141 <= value and: [value <= 8r172]) or: [8r101 <= value and: [value <= 8r132]].
