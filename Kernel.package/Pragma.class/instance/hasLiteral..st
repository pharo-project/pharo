testing
hasLiteral: aLiteral
	^keyword == aLiteral 
	   or: [arguments hasLiteral: aLiteral]