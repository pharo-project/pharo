tests
testRemoveAllInternal
	aPackage clearDependencies.
	aPackage add: (DAPackageDependency from:aPackage to:aPackage).
	aPackage add: (DAPackageDependency from:aSecondPackage to:aSecondPackage).
	aPackage removeAllInternal.
	self assert: (aPackage dependencies isEmpty)