testing
isAtLineEnd
	^ self isAtEnd or: [ next isLineSeparator ]