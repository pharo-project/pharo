tests
testNoDefaultMajorVersionFails

	self should: [ PBVersionInfo new ] raise: Error