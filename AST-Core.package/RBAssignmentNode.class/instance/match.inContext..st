matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	^(variable match: aNode variable inContext: aDictionary)
		and: [value match: aNode value inContext: aDictionary]