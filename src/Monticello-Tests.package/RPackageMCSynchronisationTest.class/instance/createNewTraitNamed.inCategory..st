private
createNewTraitNamed: aName inCategory: aCategoryName
	
	| trait |
	trait := Trait named: aName asSymbol
			uses: {}
			category: aCategoryName.
	^ trait