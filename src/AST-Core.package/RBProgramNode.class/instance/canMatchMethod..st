testing-matching
canMatchMethod: aCompiledMethod 
	^self sentMessages allSatisfy: 
			[:each | 
			(self class optimizedSelectors includes: each) 
				or: [aCompiledMethod refersToLiteral: each]]