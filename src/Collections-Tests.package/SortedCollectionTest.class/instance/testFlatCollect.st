tests - enumerating
testFlatCollect
	self assert: (#(1 2) asSortedCollection flatCollect: [ :x | { x } ]) = #(1 2) asOrderedCollection