comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [ ^ false ].
	self selector = anObject selector ifFalse: [ ^ false ].
	self arguments size = anObject arguments size ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second withMapping: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true