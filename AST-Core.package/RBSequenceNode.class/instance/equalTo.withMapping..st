comparing
equalTo: anObject withMapping: aDictionary 
	self class = anObject class ifFalse: [^false].
	self statements size = anObject statements size ifFalse: [^false].
	self statements
		with: anObject statements
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	aDictionary values asSet size = aDictionary size ifFalse: [^false].	"Not a one-to-one mapping"
	self temporaries
		do: [:each | aDictionary removeKey: each name ifAbsent: []].
	^true