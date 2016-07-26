tests
testAllSeen
	aPackageRelationGraph addPackage: packageKernel.
	self deny: (aPackageRelationGraph allSeen).
	aPackageRelationGraph computeStaticDependencies.
	self deny: (aPackageRelationGraph allSeen)