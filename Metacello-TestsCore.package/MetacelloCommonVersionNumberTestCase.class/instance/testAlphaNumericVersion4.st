test alpha/numeric version numbers
testAlphaNumericVersion4

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.9')).
