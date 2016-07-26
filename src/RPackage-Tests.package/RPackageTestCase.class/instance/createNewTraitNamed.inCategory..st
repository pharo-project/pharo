utility
createNewTraitNamed: aName inCategory: cat
	
	| cls |
	cls := Trait named: aName
			uses: {}
			category: cat.
	createdClasses add: cls.
	createdCategories add: cat.
	^ cls