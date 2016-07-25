tests
testVersion21

	self deny: (MetacelloVersionNumber fromString: '') > (MetacelloVersionNumber fromString: '0').
	self assert: (MetacelloVersionNumber fromString: '') < (MetacelloVersionNumber fromString: '0').
	self assert: (MetacelloVersionNumber fromString: '') = (MetacelloVersionNumber fromString: '').

