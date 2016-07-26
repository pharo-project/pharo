accessing-packages
selectNoPackages
	packagesSelected := Set new.
	self changed: #allSelections; changed: #packageSelected; updateClasses