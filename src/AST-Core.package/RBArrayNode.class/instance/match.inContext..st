matching
match: aNode inContext: aDictionary
	aNode class = self class
		ifFalse: [ ^ false ].
	self statements with: aNode statements do: [ :first :second | 
		(first match: second inContext: aDictionary) ifFalse: [ ^ false ] ].
	^ true