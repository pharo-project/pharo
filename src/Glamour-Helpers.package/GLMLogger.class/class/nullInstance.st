accessing
nullInstance 
	^ nullInstance ifNil: [
		nullInstance := GLMNullLogger new ]