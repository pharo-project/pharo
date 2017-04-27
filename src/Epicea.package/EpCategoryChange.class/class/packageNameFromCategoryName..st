private
packageNameFromCategoryName: aName

	self flag: #todo. "Is it really necessary to know the package name?"
	^ (RPackage organizer packageMatchingExtensionName: aName) 
		ifNotNil: [ :package | package name ]
		ifNil: [ aName ]