comparing
equalTo: anObject withMapping: aDictionary 
	^self class = anObject class 
		and: [self value equalTo: anObject value withMapping: aDictionary]