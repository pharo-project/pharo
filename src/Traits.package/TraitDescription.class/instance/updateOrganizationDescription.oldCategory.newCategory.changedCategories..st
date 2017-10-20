organization updating
updateOrganizationDescription: aTraitMethodDescription oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil changedCategories: changedCategories
	| sel effectiveCategory currentCategory |
	sel := aTraitMethodDescription selector.
	
	(self includesLocalSelector: sel) ifTrue: [ ^ self ].
	 
	currentCategory := self organization categoryOfElement: sel.
	effectiveCategory := aTraitMethodDescription effectiveMethodCategoryCurrent: currentCategory new: newCategoryOrNil.
	
	effectiveCategory ifNil: [ 
		currentCategory ifNotNil: [ changedCategories add: currentCategory ].
		^ self organization removeElement: sel ].
		
	((currentCategory isNil 
		or: [ 
				currentCategory == Protocol ambiguous 
				or: [ currentCategory == oldCategoryOrNil ] ])
		and: [ currentCategory ~~ effectiveCategory ])
		ifTrue: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization classify: sel under: effectiveCategory suppressIfDefault: false ]