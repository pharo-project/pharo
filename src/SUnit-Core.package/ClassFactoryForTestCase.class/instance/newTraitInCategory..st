creating
newTraitInCategory: category
	^ self
		newTraitNamed: self newTraitName
		uses: Array new
		category: category asSymbol.