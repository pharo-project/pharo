private-parsing
parsePrimitiveValueLiteral
	| token |
	token := currentToken.
	self step.

	^self literalValueNodeClass 
		value: token value 
		start: token start
		stop: token stop
		source: token source
	