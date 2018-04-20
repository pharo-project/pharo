instance-creation
onEnvironment: aRingEnvironment 
	^ self new
		environment: aRingEnvironment;
		initializeIndexes;
		yourself.