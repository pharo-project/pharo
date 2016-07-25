testing
needsParenthesis
	^parent 
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]