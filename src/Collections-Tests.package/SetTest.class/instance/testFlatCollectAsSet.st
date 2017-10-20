tests - enumerating
testFlatCollectAsSet
	self assert: (#(1 2) asSet flatCollect: [ :x | 1 to: (2 * x) ]) = #(1 2 3 4) asSet