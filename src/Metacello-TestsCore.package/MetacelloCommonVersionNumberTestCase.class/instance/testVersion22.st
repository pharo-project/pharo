tests
testVersion22

	self assert: (MetacelloVersionNumber fromString: '3.0.0') collapseZeros size = 1.
	self assert: (MetacelloVersionNumber fromString: '3.0.0') = (MetacelloVersionNumber fromString: '3.0').
	self assert: (MetacelloVersionNumber fromString: '3.0') = (MetacelloVersionNumber fromString: '3.0.0').

