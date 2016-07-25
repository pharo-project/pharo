running
testResourceCollection
	self assert: self class buildSuiteFromSelectors resources size = self resources size
		description: 'The suite should have the same number of resources as its test'.
	self class buildSuiteFromSelectors resources do:
		[:each |
		self assert: (self resources includes: each)
			description: each name , ':  I have this resource but my suite does not'].