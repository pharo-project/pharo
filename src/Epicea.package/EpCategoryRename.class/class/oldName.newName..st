instance creation
oldName: oldName newName: newName

	^self
		oldName: oldName
		newName: newName
		packageName: (self packageNameFromCategoryName: newName)