instance creation
on: aStream globalEnvironment: aDictionary
	
	^self basicNew
		initializeOn: aStream
		globalEnvironment: aDictionary;
		yourself.