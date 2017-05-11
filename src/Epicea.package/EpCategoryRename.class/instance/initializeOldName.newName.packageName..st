accessing
initializeOldName: theOldName newName: theNewName packageName: aPackageName

	self initializeNamed: theNewName packageName: aPackageName.
	oldCategoryName := theOldName.