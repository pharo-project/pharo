traits
removeAlias: aSymbol of: aTrait
	self setTraitComposition: (
		self traitComposition copyWithoutAlias: aSymbol of: aTrait)