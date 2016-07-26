tests
testVersion13

	self deny: ((self versionClass fromString: '0.8') <= (self versionClass fromString: '0.7')).
	self deny: ((self versionClass fromString: '0.8.1.8') <= (self versionClass fromString: '0.7.0.5')).
	