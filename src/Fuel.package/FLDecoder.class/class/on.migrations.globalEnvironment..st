instance creation
on: aStream migrations: aCollection globalEnvironment: aDictionary
	
	^self basicNew
		initializeOn: aStream 
		migrations: aCollection 
		globalEnvironment: aDictionary;
		yourself.