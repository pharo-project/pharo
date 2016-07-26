private-parsing
parseBinaryPragma
	| binaryToken |
	currentToken isBinary 
		ifFalse: [ ^ self parserError: 'Message pattern expected' ].
	binaryToken := currentToken.
	self step.
	^ self pragmaNodeClass
		selector: binaryToken value asSymbol
		keywordsPositions: (Array with: binaryToken start)
		arguments: (Array with: self parsePragmaLiteral)