operations
guessTypeWith: anECContext 
	^ (anECContext guessTempVarClass: contents type: self type) 
		ifNil: [anECContext guessArgument: contents]