tests
testVersion24

	self assert: (MetacelloVersionNumber fromString: '3.0.3.-rc.1') ~= (MetacelloVersionNumber fromString: '3.0.0.3-rc.1').
	self assert: (MetacelloVersionNumber fromString: '1.0.0.1.0.0') = (MetacelloVersionNumber fromString: '1.0.0.1').
	self assert: (MetacelloVersionNumber fromString: '1.0.0.1') ~= (MetacelloVersionNumber fromString: '1..1').

