running
runEnvironment: anEnvironment
	^ self new 
		initializeOn: anEnvironment; 
		run