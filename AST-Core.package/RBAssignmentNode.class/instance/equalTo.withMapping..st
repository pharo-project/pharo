comparing
equalTo: anObject withMapping: aDictionary 
	^self class = anObject class and: 
			[(self variable equalTo: anObject variable withMapping: aDictionary) 
				and: [self value equalTo: anObject value withMapping: aDictionary]]