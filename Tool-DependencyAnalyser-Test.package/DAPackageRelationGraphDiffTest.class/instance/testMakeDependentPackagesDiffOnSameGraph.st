running
testMakeDependentPackagesDiffOnSameGraph
	packageRelationGraphDiff makePackagesDiff.
		
	self assert: packageRelationGraphDiff dependentPackagesDiff isEmpty
	