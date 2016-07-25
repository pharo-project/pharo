accessing-packages
selectInversePackages
	packagesSelected := packages asSet 
		removeAll: packagesSelected;
		yourself.
	self changed: #allSelections; changed: #packageSelected; updateClasses