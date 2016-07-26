creating
newTraitUsing: aTraitComposition.
	^self 
		newTraitNamed: self newTraitName 
		uses: aTraitComposition 
		category: self defaultCategoryPostfix.