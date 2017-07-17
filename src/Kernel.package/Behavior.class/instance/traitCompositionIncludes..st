traits
traitCompositionIncludes: aTrait
	^self == aTrait or: 
		[self hasTraitComposition and: 
			[self traitComposition allTraits includes: aTrait]]