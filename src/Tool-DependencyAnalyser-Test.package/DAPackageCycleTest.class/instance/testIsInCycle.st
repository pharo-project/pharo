tests
testIsInCycle
	aPDPackageCycle addPackage: packageA.
	self assert: (aPDPackageCycle isInCycle: packageA).