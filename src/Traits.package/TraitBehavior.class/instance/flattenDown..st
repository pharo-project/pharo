traits
flattenDown: aTrait
| selectors |
	[self hasTraitComposition and: [self traitComposition allTraits includes: aTrait]] assert.
	(self traitComposition transformationOfTrait: aTrait)
		ifNotNil: [ :transformation | 
			selectors := transformation selectors.
	self basicLocalSelectors: (self basicLocalSelectors ifNil: Array new) asArray , selectors.
	self removeFromComposition: aTrait. ].