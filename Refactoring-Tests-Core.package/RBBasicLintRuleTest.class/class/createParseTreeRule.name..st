instance creation
createParseTreeRule: codeStrings name: aName 
	^self createParseTreeRule: codeStrings
		method: false
		name: aName