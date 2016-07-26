matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	^value match: aNode value inContext: aDictionary