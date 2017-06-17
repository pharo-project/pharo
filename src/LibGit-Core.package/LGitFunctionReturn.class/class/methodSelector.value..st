instance creation
methodSelector: aSymbol value: anObject
	^ self new
		methodSelector: aSymbol;
		value: anObject;
		yourself