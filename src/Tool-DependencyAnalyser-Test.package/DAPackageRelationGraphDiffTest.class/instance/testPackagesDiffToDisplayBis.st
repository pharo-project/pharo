running
testPackagesDiffToDisplayBis
	| first  |
	first := newRelationGraph seenPackages first.
	first clearDependencies.
	packageRelationGraphDiff make.
		
	self assert: (packageRelationGraphDiff packagesDiffToDisplay size = 1).
