tests parsing
testCascade
	self assert: (RBParser parseExpression: ' self msg; yourself') isCascade.
	self assert: (RBParser parseExpression: ' self msg:(arg msg:arg); yourself') isCascade.
	self should: [ RBParser parseExpression: ' (self msg); yourself' ] raise: SyntaxErrorNotification.
	self should: [ RBParser parseExpression: ' self ; yourself' ] raise: SyntaxErrorNotification.
	self should: [ RBParser parseExpression: ' (self) ; yourself' ] raise: SyntaxErrorNotification.