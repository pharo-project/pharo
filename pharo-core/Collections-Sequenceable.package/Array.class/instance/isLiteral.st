testing
isLiteral
	^self class == Array and: [self allSatisfy: [:each | each isLiteral]]