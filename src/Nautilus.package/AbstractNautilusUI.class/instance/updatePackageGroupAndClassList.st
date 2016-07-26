private
updatePackageGroupAndClassList

	self resetClassesList.
	self changed: #getPackageItem:.
	self changed: #getClassItem: