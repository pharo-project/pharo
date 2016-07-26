tests
testDependantPackages
	| aCollection |
	
	aPackage add: (DAPackageDependency from:aPackage to:packageA).
	
	aCollection := aPackage dependentPackages.
	self assert: (aCollection includes: packageA).
	self assert: (aCollection includes: aSecondPackage).
	self assert: (aCollection size = 2).
	
	
	