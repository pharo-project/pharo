private
step
	(currentToken notNil and: [currentToken comments notNil]) 
		ifTrue: [self extractCommentsFrom: currentToken].
	nextToken notNil 
		ifTrue: 
			[currentToken := nextToken.
			nextToken := nil]
		ifFalse: [currentToken := scanner next].