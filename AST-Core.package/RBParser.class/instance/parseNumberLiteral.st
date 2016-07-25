private-parsing
parseNumberLiteral
	| token |
	token := currentToken.
	self step.
	
	^self literalValueNodeClass 
		value: token value 
		start: token start
		stop: token stop
		source: token source
	