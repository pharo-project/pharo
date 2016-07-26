matching
match: aNode inContext: aDictionary 
	(wrappedNode match: aNode inContext: aDictionary) ifFalse: [^false].
	^super match: aNode inContext: aDictionary