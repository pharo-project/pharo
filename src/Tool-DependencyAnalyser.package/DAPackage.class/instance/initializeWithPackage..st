initialization
initializeWithPackage: aRPackageSet
	self initialize.
	self clearDependencies.
	rPackageSet := aRPackageSet.
	included := false.
	inStack := false.
	seen := false.
	tarjanIndex := -1.
	tarjanLowLink := -1.
