matching
matchLiteral: aNode inContext: aDictionary 
	^aNode isLiteralNode
		and: [(aDictionary at: self ifAbsentPut: [aNode]) = aNode]