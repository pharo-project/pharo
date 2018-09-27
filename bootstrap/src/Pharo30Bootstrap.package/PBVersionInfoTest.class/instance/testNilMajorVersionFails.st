tests
testNilMajorVersionFails

	self should: [ PBVersionInfo major: nil ] raise: Error