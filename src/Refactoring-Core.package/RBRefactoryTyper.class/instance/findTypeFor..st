computing types
findTypeFor: selectorCollection 
	^selectorCollection inject: self rootClasses
		into: 
			[:classes :each | 
			self refineTypes: classes
				with: (selectorLookup at: each ifAbsentPut: [self implementorsOf: each])]