traits
addExclusionOf: aSymbol to: aTrait
	self setTraitComposition: (
		self traitComposition copyWithExclusionOf: aSymbol to: aTrait)