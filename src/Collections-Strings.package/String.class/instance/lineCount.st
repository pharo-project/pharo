accessing
lineCount
	"Answer the number of lines represented by the receiver, where every line delimiter CR, LF or CRLF pair adds one line."

	| lineCount |
	lineCount := 0.
	self lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineCount := lineCount + 1].
	^lineCount