matching
copyInContext: aDictionary 
	^ self class statements: (self copyList: self statements inContext: aDictionary)