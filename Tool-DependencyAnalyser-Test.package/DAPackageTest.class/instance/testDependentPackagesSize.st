tests
testDependentPackagesSize
	| dependency |
	self assert: aPackage dependentPackagesSize = 1.
	dependency := DAPackageDependency from: aPackage to: packageA.
	aPackage add: dependency.
	self assert: aPackage dependentPackagesSize = 2