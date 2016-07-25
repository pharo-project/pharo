running
testMakePackagesDiffOnSameGraph
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff packagesDiff size = oldRelationGraph seenPackages size).
	self assert: (packageRelationGraphDiff packagesDiff allSatisfy: [ :item | item isUnderteminate  ]).