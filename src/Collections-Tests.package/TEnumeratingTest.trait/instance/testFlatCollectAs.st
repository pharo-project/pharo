tests - enumerating
testFlatCollectAs

	self 
		assert: (self simpleCollection flatCollect: [ :x | { x }, { x } ] as: IdentitySet) 
		equals: self simpleCollection asIdentitySet