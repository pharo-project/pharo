instance creation
fromString: aString
	^ self new
		initializeFromString: aString;
		yourself