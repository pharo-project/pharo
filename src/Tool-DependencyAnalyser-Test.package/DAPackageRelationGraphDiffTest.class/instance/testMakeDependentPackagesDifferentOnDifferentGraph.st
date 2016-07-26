running
testMakeDependentPackagesDifferentOnDifferentGraph
	| oldSize |
	oldSize := newRelationGraph seenPackages first dependentPackages size.
	newRelationGraph seenPackages first clearDependencies.

	packageRelationGraphDiff make.
		
	self assert: (packageRelationGraphDiff dependentPackagesDiffRemoved size = oldSize).
	self assert: (packageRelationGraphDiff dependentPackagesDiffUnderteminate size = (newRelationGraph seenPackages last dependentPackages size))
	