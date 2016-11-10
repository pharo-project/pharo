testing
hasLiteral: aLiteral
	^selector == aLiteral 
	   or: [arguments hasLiteral: aLiteral]