accessing
includesSubstring: aString caseSensitive: caseSensitive
	
	^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0