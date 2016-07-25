tests
testCyclesWithAtLeast
	| aCollectionOfPDPackage |
	aCollectionOfPDPackage  := OrderedCollection new.
	aCollectionOfPDPackage 
		add: packageA;
		add: packageB.

	cycleA addPackage: packageA;
			addPackage: packageB.
	
	cycleB addPackage: packageC;
			addPackage: packageD;
			addPackage: packageE.
	
	cycleC addPackage: packageE.
	
	aPackageCycleDetection addCycle: cycleA;
		addCycle: cycleB;
		addCycle: cycleC.
		
	self assert: ((aPackageCycleDetection cyclesWithAtLeast: aCollectionOfPDPackage )size = 1).
	