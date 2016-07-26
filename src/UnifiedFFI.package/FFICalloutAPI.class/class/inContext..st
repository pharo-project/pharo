instance creation
inContext: aContext
	^ self basicNew 
		initializeContext: aContext;
		yourself