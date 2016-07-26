matching
match: aNode inContext: aDictionary 
	aNode class = self class ifFalse: [^false].
	self selector = aNode selector ifFalse: [^false].
	(receiver match: aNode receiver inContext: aDictionary) ifFalse: [^false].
	self arguments
		with: aNode arguments
		do: [:first :second | (first match: second inContext: aDictionary) ifFalse: [^false]].
	^true