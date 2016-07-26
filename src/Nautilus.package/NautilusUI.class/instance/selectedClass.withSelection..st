private
selectedClass: aClass withSelection: aPackage
	self model
		package: aPackage
		class: aClass.
	self resetPackageGroupsListSelection.
	self packageWidget selectPackage: aPackage.
	self updatePackageGroupAndClassList