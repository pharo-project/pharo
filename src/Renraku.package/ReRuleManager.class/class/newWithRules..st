instance creation
newWithRules: aCollection 
	^ self basicNew
		initializeRules: aCollection;
		yourself