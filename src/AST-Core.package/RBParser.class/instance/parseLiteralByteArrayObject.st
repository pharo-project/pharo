private-parsing
parseLiteralByteArrayObject
	(currentToken isLiteralToken and: 
			[currentToken value isInteger and: [currentToken value between: 0 and: 255]]) 
		ifFalse: [
			| errorNode |
			errorNode := self parserError: 'Expecting 8-bit integer'.
			self step.
			^ errorNode ].
	^self parsePrimitiveLiteral