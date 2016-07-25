tests
testSeenPackagesName
	aPackageRelationGraph addPackage: packageKernel.
	self deny: (aPackageRelationGraph seenPackagesName includes: 'Kernel').
	aPackageRelationGraph computeStaticDependencies.
	self assert: (aPackageRelationGraph seenPackagesName includes: 'Kernel').