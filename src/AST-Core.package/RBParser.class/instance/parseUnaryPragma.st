private-parsing
parseUnaryPragma
	| selector |
	selector := currentToken.
	self step.
	^ self pragmaNodeClass selector: selector value asSymbol keywordsPositions: (Array with: selector start) arguments: #()