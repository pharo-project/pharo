instance creation
withArguments: aCollection
	^ self basicNew 
		initializeWithArguments: aCollection;
		yourself