organization updating
noteRecategorizedSelector: aSymbol from: oldCategoryOrNil to: newCategoryOrNil
	| changedCategories |
	changedCategories := self updateOrganizationSelector: aSymbol oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil.
	changedCategories do: [:each |
		(self organization isEmptyCategoryNamed: each) ifTrue: [self organization removeCategory: each]]