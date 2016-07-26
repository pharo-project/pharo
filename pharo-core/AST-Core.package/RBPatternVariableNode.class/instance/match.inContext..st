matching
match: aNode inContext: aDictionary 
	self isAnything 
		ifTrue: [^(aDictionary at: self ifAbsentPut: [aNode]) = aNode].
	self isLiteralNode ifTrue: [^self matchLiteral: aNode inContext: aDictionary].
	self isStatement 
		ifTrue: [^self matchStatement: aNode inContext: aDictionary].
	(aNode isKindOf: self matchingClass) ifFalse: [^false].
	^(aDictionary at: self ifAbsentPut: [aNode]) = aNode