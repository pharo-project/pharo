tests
testUnixAbsolutePathName

	self assert: (Path from: '/test') isAbsolute.
	self assert: (Path from: '/etc/bin') isAbsolute.