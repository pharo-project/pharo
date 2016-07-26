searching
makePackagesDiff
	"lookUp for new and remove packages"

	self
		lookUpForRemovedPackages;
		lookUpForNewPackages;
		 lookUpForUndeterminatePackages.