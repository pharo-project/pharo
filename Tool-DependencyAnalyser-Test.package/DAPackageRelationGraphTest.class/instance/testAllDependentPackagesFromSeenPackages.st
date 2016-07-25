tests
testAllDependentPackagesFromSeenPackages
	packageA
		add: (self packageDependencyFrom: packageA to: packageB);
		add: (self packageDependencyFrom: packageA to: packageC).
	packageB add: (self packageDependencyFrom: packageB to: packageC).
	packageB add: (self packageDependencyFrom: packageB to: packageA).
	
	packageA beSeen.
	packageB beSeen.
	aPackageRelationGraph addPackage: packageA;
							addPackage: packageB.
	self assert: (aPackageRelationGraph allDependentPackagesFromSeenPackages size = 3).