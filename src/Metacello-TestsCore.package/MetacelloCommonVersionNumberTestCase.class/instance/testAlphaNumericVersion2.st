test alpha/numeric version numbers
testAlphaNumericVersion2

	self assert: ((self versionClass fromString: '2.9.0-alpha.2') < (self versionClass fromString: '2.9.0-alpha.3')).
