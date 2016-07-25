matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	^self 
		matchList: messages
		against: aNode messages
		inContext: aDictionary