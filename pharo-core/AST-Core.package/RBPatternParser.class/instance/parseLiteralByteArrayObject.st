private-parsing
parseLiteralByteArrayObject
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: 
			[node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
			node isLiteralNode 
				ifTrue: 
					[self step.
					^node]].
	^super parseLiteralByteArrayObject