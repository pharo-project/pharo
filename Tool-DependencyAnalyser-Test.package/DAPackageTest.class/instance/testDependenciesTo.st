tests
testDependenciesTo
	self assert: (aPackage dependenciesTo: aSecondPackage packageName) size = 1.
	self assert: (aPackage dependenciesTo: 'Test') isEmpty