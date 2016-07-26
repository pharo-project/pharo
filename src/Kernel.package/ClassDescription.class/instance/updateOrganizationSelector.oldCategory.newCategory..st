organization updating
updateOrganizationSelector: aSymbol oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil
	| changedCategories composition |
	changedCategories := IdentitySet new.
	
	composition := self hasTraitComposition
		ifTrue: [ self traitComposition ]
		ifFalse: [ TraitComposition new ].
		
	(composition methodDescriptionsForSelector: aSymbol)
		do: [ :each | 
			self
				updateOrganizationDescription: each
				oldCategory: oldCategoryOrNil
				newCategory: newCategoryOrNil
				changedCategories: changedCategories ].
	^ changedCategories