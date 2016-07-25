parsing
parseRewriteExpression: aString onError: aBlock 
	^RBPatternParser parseExpression: aString onError: aBlock