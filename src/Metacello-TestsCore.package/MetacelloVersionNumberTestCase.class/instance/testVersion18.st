tests
testVersion18

	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1')).
	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1-0')).
