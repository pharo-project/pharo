private-parsing
parseBinaryMessageWith: aNode 
	| binaryToken |
	binaryToken := currentToken.
	self step.
	^self messageNodeClass 
		receiver: aNode
		selector:  binaryToken value
		keywordsPositions: (Array with: binaryToken start)
		arguments: (Array with: self parseUnaryMessage)