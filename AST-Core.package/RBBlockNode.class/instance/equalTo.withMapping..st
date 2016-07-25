comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second |	(first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	(self body equalTo: anObject body withMapping: aDictionary)
		ifFalse: [^false].
	self arguments do: [:each | aDictionary removeKey: each name].
	^true