traits
flattenDown: aTrait
	| selectors |
	[self hasTraitComposition and: [self traitComposition allTraits includes: aTrait]] assert.
	selectors := (self traitComposition transformationOfTrait: aTrait) selectors.
	self basicLocalSelectors: self basicLocalSelectors , selectors.
	self removeFromComposition: aTrait.