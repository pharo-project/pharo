running
testHasChangedForPackageItemDiff
	| first second |
	first := newRelationGraph seenPackages first.
	second := newRelationGraph seenPackages last.
	
	first clearDependencies.
	
	packageRelationGraphDiff make.
			
	self assert: ((packageRelationGraphDiff packageDiffNamed: first packageName) hasChanged).
	self deny: ((packageRelationGraphDiff packageDiffNamed: second packageName) hasChanged).
