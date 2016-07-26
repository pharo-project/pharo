private-parsing
parseBinaryPattern
	| binaryToken node args |
	currentToken isBinary
		ifFalse: [ ^ self parserError: 'Message pattern expected'].
	binaryToken := currentToken.
	self step.
	args := Array with: self parseVariableNode.
	node := self methodNodeClass
		selector: binaryToken value asSymbol
		keywordsPositions: (Array with: binaryToken start)
		arguments: args.
	node comments: node comments , args last comments.
	args last comments ifNotEmpty: [ args last comments: #() ].
	^node