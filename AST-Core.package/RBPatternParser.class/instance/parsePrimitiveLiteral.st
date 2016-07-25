private-parsing
parsePrimitiveLiteral
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: 
			[node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
			node isLiteralNode 
				ifTrue: 
					[self step.
					^node].
			currentToken := RBLiteralToken 
						value: currentToken value asSymbol
						start: currentToken start
						stop: currentToken stop].
	^super parsePrimitiveLiteral