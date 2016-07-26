instance creation
rules: aCollection name: aString
	^ self new
		rules: aCollection;
		name: aString;
		yourself