matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	^(self 
		matchList: arguments
		against: aNode arguments
		inContext: aDictionary) 
			and: [body match: aNode body inContext: aDictionary]