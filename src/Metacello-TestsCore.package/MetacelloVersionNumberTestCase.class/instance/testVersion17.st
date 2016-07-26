tests
testVersion17

	self assert: ((self versionClass fromString: '1.0') = (self versionClass fromString: '1.0.0')).
	self assert: ((self versionClass fromString: '1') = (self versionClass fromString: '1.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1.0-beta.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-beta.0')).
	self assert: ((self versionClass fromString: '1') > (self versionClass fromString: '1-beta')).
