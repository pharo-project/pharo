private-parsing
parseUnaryPattern
	| selector |
	selector := currentToken.
	self step.
	^self methodNodeClass selector: selector value asSymbol keywordsPositions: (Array with: selector start) arguments: #()