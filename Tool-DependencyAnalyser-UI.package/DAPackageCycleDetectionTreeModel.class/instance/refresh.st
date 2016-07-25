actions
refresh
	self analysis refresh.
	self filter ifNil: [ self cycles: self analysis cycles ]
		ifNotNil: [ self cycles: (self analysis cyclesWithAtLeast: filter) ].
	self buildRoots