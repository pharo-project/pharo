initialization
initializeNamed: aName
	self initialize.
	classCategoryName := aName.
	affectedPackageName := self packageNameFromCategoryName: aName