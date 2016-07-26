running
testPackagesDiffToDisplay
	| first second |
	first := newRelationGraph seenPackages first.
	second := newRelationGraph seenPackages last.
	
	first clearDependencies.
	second dependencies remove: (second dependencies anyOne).
	
	packageRelationGraphDiff make.
	self assert: (packageRelationGraphDiff packagesDiffToDisplay size = 2).
