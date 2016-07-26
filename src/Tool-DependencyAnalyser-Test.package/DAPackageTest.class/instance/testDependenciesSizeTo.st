running
testDependenciesSizeTo
	packageA
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageC).
	self assert: ((packageA dependenciesSizeTo: packageB )= 1).