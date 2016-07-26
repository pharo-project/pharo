token testing
isAssignment
	^ currentToken = ':='
		or: [currentToken = '_']