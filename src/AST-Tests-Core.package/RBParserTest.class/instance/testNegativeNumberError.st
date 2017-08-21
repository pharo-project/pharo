tests parsing
testNegativeNumberError
	self should: [ self parserClass parseExpression: '- 2' ] raise: SyntaxErrorNotification