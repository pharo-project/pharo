instance creation
oldName: oldPackageName newName: newPackageName
	| newInstance |
	newInstance := self named: newPackageName.
	^ newInstance oldCategoryName: oldPackageName;
		yourself.