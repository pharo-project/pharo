matching
match: aNode inContext: aDictionary 
	self class = aNode class ifFalse: [^false].
	^(self 
		matchList: temporaries
		against: aNode temporaries
		inContext: aDictionary) and: 
				[self 
					matchList: statements
					against: aNode statements
					inContext: aDictionary]