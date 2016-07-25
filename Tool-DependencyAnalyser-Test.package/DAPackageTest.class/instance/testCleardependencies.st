tests
testCleardependencies
	aPackage clearDependencies.
	self assert: (aPackage dependencies isEmpty)