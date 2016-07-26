tests - enumerating
testFlatCollectAsSet

	self 
		assert: (self simpleCollection flatCollectAsSet: [ :x | { x }, { x } ]) 
		equals: self simpleCollection asSet