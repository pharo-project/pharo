tests
testCycleAlreadyFound
	| cycle otherCycle |
	cycle := DAPackageCycle new.
	cycle addPackage: packageA.
	cycle addPackage: packageB.
	aPackageCycleDetection addCycle: cycle.
	otherCycle := DAPackageCycle new.
	otherCycle addPackage: packageA.
	self deny: (aPackageCycleDetection cycleAlreadyFound: otherCycle).
	otherCycle addPackage: packageB.
	self assert: (aPackageCycleDetection cycleAlreadyFound: otherCycle).