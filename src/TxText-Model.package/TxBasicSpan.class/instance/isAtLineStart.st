testing
isAtLineStart
	^ self isAtStart or: [ self prev isLineSeparator ]