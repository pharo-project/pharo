traits-factory
newTraitSuffixed: suffix

	| trait |
	trait := Trait 
		named: (self traitNamePrefix, suffix) asSymbol
		uses: Array new 
		category: (self classFactory packageName, '-', self classFactory defaultCategoryPostfix) asSymbol.
	self traits add: trait.
	
	^ trait