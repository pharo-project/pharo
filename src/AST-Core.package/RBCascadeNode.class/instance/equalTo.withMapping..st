comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages
		with: anObject messages
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true