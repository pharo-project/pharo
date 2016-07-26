visiting
visitLineSeparator: aSpan

	"A line separator indicates new line
	if current span position = 0 then we finish scanning,
	else we treat it as beginning of a new line, and must continue"
	currentPosition isAtSpanStart ifTrue: [
		"end of line"
		self completeLine.
		^ currentPosition
	].

	^ currentPosition nextPosition