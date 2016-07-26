matching
copyInContext: aDictionary 
	^ self class new
		variable: (self variable copyInContext: aDictionary);
		value: (self value copyInContext: aDictionary);
		yourself