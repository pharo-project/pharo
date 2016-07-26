instance creation
methodSearch: aString 
	^(self new)
		methodSearchString: aString;
		yourself