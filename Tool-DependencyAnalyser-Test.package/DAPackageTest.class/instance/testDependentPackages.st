tests
testDependentPackages
	self assert: (aPackage dependentPackages size = 1).
	self assert: (aPackage dependentPackages anySatisfy: [ :package | package = aSecondPackage ]).
	self assert: (aSecondPackage dependentPackages isEmpty)