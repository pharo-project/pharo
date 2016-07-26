matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	self isForByteArray = aNode isForByteArray ifFalse: [^false].
	^self 
		matchList: contents
		against: aNode contents
		inContext: aDictionary