tests
testInstanceReuse
	| x m n y |
	x := (MCPackage new name: self mockCategoryName) snapshot.
	Smalltalk garbageCollect.
	n := MCDefinition allSubInstances size.
	y := (MCPackage new name: self mockCategoryName) snapshot.
	Smalltalk garbageCollect.
	m := MCDefinition allSubInstances size.
	self assert: m = n