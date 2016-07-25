tests
testInitializeFrom
	aPDPackageDependencyWrapper := DAPackageDependencyWrapper newWith: packageA.
	self assert: (aPDPackageDependencyWrapper daPackage = packageA).