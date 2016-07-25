testing
isAtLineStart
	^ self isAtStart or: [ self span isLineSeparator and: [ self isAtSpanEnd ] ]