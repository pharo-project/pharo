matching
copyInContext: aDictionary 
	^ self class new
		value: (self value copyInContext: aDictionary);
		yourself