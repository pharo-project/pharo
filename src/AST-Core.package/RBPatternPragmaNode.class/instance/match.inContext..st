matching
match: aNode inContext: aDictionary
	aNode class = self matchingClass
		ifFalse: [ ^ false ].
	self isSelectorList ifTrue: [
		^ (aDictionary at: self selectorParts first ifAbsentPut: [ aNode selector ]) = aNode selector
			and: [ (aDictionary at: arguments first ifAbsentPut: [ aNode arguments ]) = aNode arguments ] ].
	^ self matchArgumentsAgainst: aNode inContext: aDictionary