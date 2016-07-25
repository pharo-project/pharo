tests
testReversedCycle
	aPDPackageCycle 
			addPackage: packageA;
			addPackage: packageB;
			addPackage: packageC;
			yourself.
	aPDPackageCycle reversedCycle.
	self assert: ((aPDPackageCycle packageAt: 1) = packageC).
	self assert: ((aPDPackageCycle packageAt: 2) = packageB).
	self assert: ((aPDPackageCycle packageAt: 3) = packageA).
				