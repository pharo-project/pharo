formatting
expandMacrosWith: anObject with: anotherObject with: thirdObject 
	^self expandMacrosWithArguments: (Array 
				with: anObject
				with: anotherObject
				with: thirdObject)