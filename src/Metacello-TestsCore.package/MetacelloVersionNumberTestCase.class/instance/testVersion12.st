tests
testVersion12

	self deny: ((self versionClass fromString: '1.0') <= (self versionClass fromString: '0.7'))