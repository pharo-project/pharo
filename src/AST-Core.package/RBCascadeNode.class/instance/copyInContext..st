matching
copyInContext: aDictionary 
	^ self class new
		messages: (self copyList: self messages inContext: aDictionary);
		yourself