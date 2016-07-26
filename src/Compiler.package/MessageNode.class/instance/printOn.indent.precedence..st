printing
printOn: strm indent: level precedence: outerPrecedence

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printOn: strm indent: level]