matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [ ^ false ].
	^ self matchList: statements against: aNode statements inContext: aDictionary