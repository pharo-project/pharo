tests
testIsAtLeastInCycle
	| aCollectionOfPDPackage |
	aCollectionOfPDPackage  := OrderedCollection new.
	aCollectionOfPDPackage 
		add: packageA;
		add: packageB;
		add: packageC.
	
	aPDPackageCycle
		addPackage: packageA;
		addPackage: packageB.
	
	self assert: (aPDPackageCycle isAtLeastInCycle: aCollectionOfPDPackage).
	aCollectionOfPDPackage remove: packageA; remove: packageB.
	self deny: (aPDPackageCycle isAtLeastInCycle: aCollectionOfPDPackage).