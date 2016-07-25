testing
needsParenthesis
	^parent 
		ifNil: [false]
		ifNotNil: 
			[self precedence > parent precedence 
				or: [self precedence = parent precedence and: [self isUnary not]]]