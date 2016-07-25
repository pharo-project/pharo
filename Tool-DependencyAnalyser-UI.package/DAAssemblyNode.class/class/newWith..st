instance creation
newWith: aCollection 
	^ self basicNew
		initializeWith: aCollection;
		yourself