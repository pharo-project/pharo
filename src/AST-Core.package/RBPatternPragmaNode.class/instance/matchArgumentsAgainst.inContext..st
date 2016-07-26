matching
matchArgumentsAgainst: aNode inContext: aDictionary 
	self arguments size = aNode arguments size
		ifFalse: [ ^ false ].
	(self matchSelectorAgainst: aNode inContext: aDictionary) 
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true