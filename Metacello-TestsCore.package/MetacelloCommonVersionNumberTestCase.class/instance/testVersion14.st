tests
testVersion14

	self assert: ((self versionClass fromString: '2.9.0-alpha02') < (self versionClass fromString: '2.9.0-alpha03')).
