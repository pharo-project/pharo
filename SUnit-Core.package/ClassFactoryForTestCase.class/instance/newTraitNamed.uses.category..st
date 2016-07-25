creating
newTraitNamed: aTraitName uses: aTraitComposition category: aCategory
	| newTrait |
	
	newTrait := Trait 
					named: aTraitName
					uses: aTraitComposition 
					category: (self packageName, '-', aCategory) asSymbol..
		
	self createdTraits add: newTrait.
	
	^newTrait.