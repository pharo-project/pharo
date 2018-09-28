test-semantic-version
testFromSemanticVersionWithInvalidMajorFails

	self should: [ PBVersionInfo fromSemanticVersion: 'Invalid-alpha' ] raise: Error.