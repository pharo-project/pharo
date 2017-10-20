tests
testEnabled
	self assert: testedInstance enabled.
	self openInstance.
	self assert: self morph enabled.
	testedInstance enabled: false.
	self deny: self morph enabled