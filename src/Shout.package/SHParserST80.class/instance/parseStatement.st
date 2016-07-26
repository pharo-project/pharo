parse
parseStatement
	currentTokenFirst == $^
		ifTrue: [self scanPast: #return].
	self parseExpression