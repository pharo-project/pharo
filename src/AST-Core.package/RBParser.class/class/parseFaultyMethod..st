parsing
parseFaultyMethod: aString 
	^self parseMethod: aString onError: self errorNodeBlock