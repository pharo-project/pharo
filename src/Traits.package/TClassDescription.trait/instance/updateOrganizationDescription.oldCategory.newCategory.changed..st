organization updating
updateOrganizationDescription: each oldCategory: newCategoryOrNil newCategory: oldCategoryOrNil changed: changedCategories
	| sel effectiveCategory currentCategory |
	
	sel := each selector.
	(self includesLocalSelector: sel) ifTrue: [ ^ self ].
	
	currentCategory := self organization categoryOfElement: sel.
	effectiveCategory := each 
		effectiveMethodCategoryCurrent: currentCategory 
		new: newCategoryOrNil.
	
	effectiveCategory 
		ifNil: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization removeElement: sel.
			^ self ].
		
	(currentCategory isNil 
		or: [ currentCategory == Protocol ambiguous 
		or: [ currentCategory == oldCategoryOrNil ] ])
	ifFalse: [ ^ self ].
	
	(currentCategory ~~ effectiveCategory)
		ifTrue: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization classify: sel under: effectiveCategory suppressIfDefault: false ]