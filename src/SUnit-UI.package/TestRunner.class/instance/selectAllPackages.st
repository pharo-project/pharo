accessing-packages
selectAllPackages
	packagesSelected := packages asSet.
	self changed: #allSelections; changed: #packageSelected; updateClasses