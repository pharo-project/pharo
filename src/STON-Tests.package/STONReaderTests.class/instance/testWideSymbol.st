tests
testWideSymbol
	self assert: (self materialize: '#''яблоко''') = #яблоко.
	self assert: (self materialize: '#яблоко') = #яблоко