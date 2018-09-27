test-default-values
testNoDefaultMajorVersionFails

	self should: [ PBVersionInfo new ] raise: Error