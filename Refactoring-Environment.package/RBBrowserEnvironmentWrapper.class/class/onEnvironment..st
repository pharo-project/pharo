instance creation
onEnvironment: anEnvironment 
	^(self basicNew)
		initialize;
		onEnvironment: anEnvironment;
		yourself