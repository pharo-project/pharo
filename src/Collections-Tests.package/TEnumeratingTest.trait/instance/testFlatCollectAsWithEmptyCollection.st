tests - enumerating
testFlatCollectAsWithEmptyCollection

	self 
		assert: (self empty flatCollect: [ :x | { x }, { x } ] as: IdentitySet) 
		equals: self empty asIdentitySet