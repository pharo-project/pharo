tests
testSizeOfCycles
	self assert: aPDPackageCycle size = 0.
	aPDPackageCycle addPackage: (DAPackage on: (RPackageSet named:'Kernel')).
	aPDPackageCycle addPackage: (DAPackage on: (RPackageSet named:'Collections-Abstract')).
	self assert: (aPDPackageCycle size = 2).