testing
intersectsInterval: anInterval 
	^(anInterval first between: self start and: self stop) 
		or: [self start between: anInterval first and: anInterval last]