matching
match: aNode inContext: aDictionary 
	aNode class = self matchingClass 
		ifFalse: [ ^ false ].
	aDictionary at: '-source-' put: aNode source.
	self isSelectorList ifTrue: [
		^ (aDictionary at:  self selectorParts first ifAbsentPut: [ aNode selector ]) = aNode selector
			and: [ (aDictionary at: arguments first ifAbsentPut: [ aNode arguments ]) = aNode arguments
			and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
			and: [ body match: aNode body inContext: aDictionary ] ] ] ].
	^(self matchArgumentsAgainst: aNode inContext: aDictionary) 
		and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
		and: [ body match: aNode body inContext: aDictionary ] ]