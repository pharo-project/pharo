running
testPackagesDiffToDisplayOnSameGraph
	
	packageRelationGraphDiff make.
			
	self assert: (packageRelationGraphDiff packagesDiffToDisplay isEmpty).
