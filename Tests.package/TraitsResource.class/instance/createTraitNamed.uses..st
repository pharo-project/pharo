utility
createTraitNamed: aSymbol uses: aTraitComposition
	| trait |
	trait := Trait
		named: aSymbol
		uses: aTraitComposition
		category: self categoryName.
	self createdClassesAndTraits add: trait.
	^trait