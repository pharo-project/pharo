matching
copyInContext: aDictionary 
	^ self class
		selector: self selector
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ])