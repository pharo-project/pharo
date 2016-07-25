instance creation
onEnvironment: anEnvironment and: anotherEnvironment 
	^(self onEnvironment: anEnvironment)
		andedEnvironment: anotherEnvironment;
		yourself