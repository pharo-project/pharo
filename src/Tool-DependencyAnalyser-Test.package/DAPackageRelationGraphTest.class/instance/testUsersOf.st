tests
testUsersOf
	packageA 
		add: (self packageDependencyFrom: packageA to:  packageB);
		add: (self packageDependencyFrom: packageA to:  packageC);
		beSeen.
	packageB
		add: (self packageDependencyFrom: packageA to:  packageC);
		beSeen.
		
	aPackageRelationGraph
		 addPackage: packageA;
		 addPackage: packageB.
		
	self assert: ((aPackageRelationGraph usersOf: packageC) size = 2).
	self assert: ((aPackageRelationGraph usersOf: packageC) includes: packageB).
			