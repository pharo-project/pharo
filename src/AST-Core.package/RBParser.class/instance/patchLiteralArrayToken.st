private
patchLiteralArrayToken
	(currentToken isIdentifier and: 
			[self nextToken isAssignment 
				and: [currentToken stop + 1 = self nextToken start]]) 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: (currentToken value , ':') asSymbol
						start: currentToken start
						stop: self nextToken start.
			nextToken := RBLiteralToken 
						value: #=
						start: nextToken stop
						stop: nextToken stop.
			^self].
	currentToken isAssignment 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: #':'
						start: currentToken start
						stop: currentToken start.
			nextToken := RBLiteralToken 
						value: #=
						start: currentToken stop
						stop: currentToken stop.
			^self].
	currentToken isSpecial 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: (String with: currentToken value) asSymbol
						start: currentToken start
						stop: currentToken stop.
			^self].
	(currentToken isIdentifier and: [currentToken value includes: $.]) 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: currentToken value
						start: currentToken start
						stop: currentToken stop.
			^self].
	(currentToken isIdentifier 
		or: [currentToken isBinary or: [currentToken isKeyword]]) 
			ifFalse: [^self parserError: 'Invalid token'].
	currentToken := RBLiteralToken 
				value: currentToken value asSymbol
				start: currentToken start
				stop: currentToken stop
				source: (source copyFrom: currentToken start to: currentToken stop).