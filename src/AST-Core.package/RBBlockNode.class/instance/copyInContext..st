matching
copyInContext: aDictionary 
	^ self class new
		arguments: (self copyList: self arguments inContext: aDictionary);
		body: (self body copyInContext: aDictionary);
		yourself