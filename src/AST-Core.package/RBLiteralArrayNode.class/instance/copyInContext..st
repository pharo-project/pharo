matching
copyInContext: aDictionary 
	^ self class 
		startPosition: 0
		contents: (self copyList: self contents inContext: aDictionary)
		stopPosition: -1
		isByteArray: isByteArray