private-parsing
parsePrimitiveIdentifier
	| token node |
	token := currentToken.
	self step.
	node := self variableNodeClass identifierNamed: token value at: token start.
	self addCommentsTo: node.
	^node