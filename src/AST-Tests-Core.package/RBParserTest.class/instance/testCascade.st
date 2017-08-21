tests parsing
testCascade
	self assert: (self parserClass parseExpression: ' self msg; yourself') isCascade.
	self assert: (self parserClass parseExpression: ' self msg:(arg msg:arg); yourself') isCascade.
	self should: [ self parserClass parseExpression: ' (self msg); yourself' ] raise: SyntaxErrorNotification.
	self should: [ self parserClass parseExpression: ' self ; yourself' ] raise: SyntaxErrorNotification.
	self should: [ self parserClass parseExpression: ' (self) ; yourself' ] raise: SyntaxErrorNotification.