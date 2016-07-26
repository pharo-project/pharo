matching
copyInContext: aDictionary 
	^self class
		receiver: (self receiver copyInContext: aDictionary)
		selector: self selector
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ]).