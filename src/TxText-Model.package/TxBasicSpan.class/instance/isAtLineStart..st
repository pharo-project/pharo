testing
isAtLineStart: spanPosition
	"just after the line start"
	^ spanPosition = 0 and: [ prev isLineSeparator ]