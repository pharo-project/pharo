comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [^false].
	self isForByteArray = anObject isForByteArray ifFalse: [^false].
	self contents size = anObject contents size ifFalse: [^false].
	self contents
		with: anObject contents
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true