running
testHasChangedForPackageItemDiffOnOneDependentPackageRemoved
	| first |
	first := newRelationGraph seenPackages first.
	first dependencies remove: (first dependencies anyOne).
	
	packageRelationGraphDiff make.
			
	self assert: ((packageRelationGraphDiff packageDiffNamed: first packageName) hasChanged).
	