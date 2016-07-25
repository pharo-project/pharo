running
testMakePackagesDiffOnDifferentGraph
	newRelationGraph removePackage: 'Collections-Abstract'.
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff packagesDiffRemovedSize = 1).
	self assert: (packageRelationGraphDiff packagesDiffUnderteminateSize = 1).