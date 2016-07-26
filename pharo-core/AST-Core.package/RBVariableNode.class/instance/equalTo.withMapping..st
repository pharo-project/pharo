comparing
equalTo: anObject withMapping: aDictionary 
	^self class = anObject class and: 
			[(aDictionary at: self name ifAbsentPut: [anObject name]) = anObject name]