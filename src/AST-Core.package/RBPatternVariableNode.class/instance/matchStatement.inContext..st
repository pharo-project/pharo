matching
matchStatement: aNode inContext: aDictionary 
	(aNode parent notNil and: [aNode parent isSequence]) ifFalse: [^false].
	^(aDictionary at: self ifAbsentPut: [aNode]) = aNode