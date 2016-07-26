private-parsing
parseUnaryMessageWith: aNode 
	| selector |
	selector := currentToken.
	self step.
	^self messageNodeClass
		receiver: aNode
		selector:  selector value
		keywordsPositions: (Array with: selector start)
		arguments: #()