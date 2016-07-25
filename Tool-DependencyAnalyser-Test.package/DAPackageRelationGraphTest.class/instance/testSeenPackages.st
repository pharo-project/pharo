tests
testSeenPackages
	aPackageRelationGraph addPackage: packageKernel.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: (aPackageRelationGraph seenPackages isEmpty).
	aPackageRelationGraph computeStaticDependencies.
	self assert: (aPackageRelationGraph seenPackages size = 2).