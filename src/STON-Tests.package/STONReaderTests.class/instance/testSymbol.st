tests
testSymbol
	self assert: (self materialize: '#''foo''') = #foo.
	self assert: (self materialize: '#foo') = #foo