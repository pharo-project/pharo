tests
testCyclesWith
	cycleA addPackage: packageA;
			addPackage: packageB.
	
	cycleB addPackage: packageC;
			addPackage: packageD;
			addPackage: packageE.
	
	cycleC addPackage: packageA;
			addPackage: packageE.
	
	aPackageCycleDetection addCycle: cycleA;
		addCycle: cycleB;
		addCycle: cycleC.
		
	self assert: ((aPackageCycleDetection cyclesWith: packageA )size = 2).
	self assert: ((aPackageCycleDetection cyclesWith: packageC )size = 1).
	self assert: ((aPackageCycleDetection cyclesWith: packageE)size = 2).
	