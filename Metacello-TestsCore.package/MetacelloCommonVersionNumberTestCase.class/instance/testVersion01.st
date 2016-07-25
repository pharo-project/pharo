tests
testVersion01

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')