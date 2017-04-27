instance creation
oldName: oldName newName: newName packageName: packageName

	^ self basicNew
		initializeOldName: oldName newName: newName packageName: packageName;
		yourself