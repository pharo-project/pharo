comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body equalTo: anObject body withMapping: aDictionary ] ]) 
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary)
			ifFalse: [ ^ false ].
		aDictionary removeKey: first name ].
	self pragmas with: anObject pragmas do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true