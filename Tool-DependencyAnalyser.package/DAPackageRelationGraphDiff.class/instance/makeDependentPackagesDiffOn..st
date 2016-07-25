searching
makeDependentPackagesDiffOn: aPackage
	
	| oldDependentPackages newDependentPackages  |
	
	oldDependentPackages := (oldRelationGraph packageAt: aPackage packageName) dependentPackages.
	newDependentPackages := (newRelationGraph packageAt: aPackage packageName) dependentPackages.
	
	self lookUpForRemovedDependentPackages: oldDependentPackages with: newDependentPackages from: aPackage.
	self lookUpForNewDependentPackages: newDependentPackages with: oldDependentPackages from: aPackage.
	self lookUpForUndeterminateDependentPackagesFrom: aPackage.