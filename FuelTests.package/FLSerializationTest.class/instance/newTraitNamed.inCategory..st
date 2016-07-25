traits-factory
newTraitNamed: traitName inCategory: aCategory

	^ Trait 
		named: traitName asSymbol
		uses: Array new 
		category: aCategory asSymbol