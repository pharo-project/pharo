private-parsing
parsePrimitiveLiteral

	currentToken isNumberLiteralToken
		ifTrue: [ ^ self parseNumberLiteral ]
		ifFalse: [ ^ self parsePrimitiveValueLiteral ]