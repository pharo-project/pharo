operations
guessTypeWith: anECContext 
	^ anECContext theClass ifNotNil: [anECContext theClass superclass]