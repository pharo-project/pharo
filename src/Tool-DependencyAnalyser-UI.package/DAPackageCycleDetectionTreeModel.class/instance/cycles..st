accessing
cycles: aCollectionOfCycles
	cycles := aCollectionOfCycles sorted: [ :a :b | a size > b size ].
	self buildRoots.