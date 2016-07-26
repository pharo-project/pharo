private-parsing
basicParsePragma
	^ currentToken isIdentifier
		ifTrue: [ self parseUnaryPragma ]
		ifFalse: [
			currentToken isKeyword
				ifTrue: [ 	
					currentToken isPrimitiveKeyword
						ifTrue: [ ^ self parsePrimitiveKeywordPragma ].
					self parseKeywordPragma ]
				ifFalse: [ self parseBinaryPragma ] ]