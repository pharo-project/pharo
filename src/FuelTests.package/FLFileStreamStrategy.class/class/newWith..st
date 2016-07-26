instance creation
newWith: aFileStream

	^self basicNew
		initializeWith: aFileStream;
		yourself