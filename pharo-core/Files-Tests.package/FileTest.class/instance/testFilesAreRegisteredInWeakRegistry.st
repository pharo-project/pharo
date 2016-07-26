tests
testFilesAreRegisteredInWeakRegistry

	|  f |
	f := (File named: 'asd.txt') writeStream.
	self assert: (File registry keys includes: f)