instance creation
errorMessage: aString value: aValue at: aPosition

	^ self new
		errorMessage: aString;
		value: aValue;
		start: aPosition;
		yourself