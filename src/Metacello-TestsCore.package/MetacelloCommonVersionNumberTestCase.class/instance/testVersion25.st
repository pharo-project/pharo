tests
testVersion25

	self assert: ((MetacelloVersionNumber fromString: '1.0-beta.24.0.1') < (MetacelloVersionNumber fromString: '1.0-beta.28')).
