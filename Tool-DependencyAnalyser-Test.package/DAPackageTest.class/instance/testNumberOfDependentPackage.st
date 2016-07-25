tests
testNumberOfDependentPackage
	self assert: (aPackage dependentPackagesSize = 1).
	aPackage add: (DAPackageDependency from: aPackage to: packageA).
	self assert: (aPackage dependentPackagesSize = 2).