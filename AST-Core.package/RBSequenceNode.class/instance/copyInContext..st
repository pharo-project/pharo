matching
copyInContext: aDictionary 
	^ self class new
		temporaries: (self copyList: self temporaries inContext: aDictionary);
		statements: (self copyList: self statements inContext: aDictionary);
		yourself