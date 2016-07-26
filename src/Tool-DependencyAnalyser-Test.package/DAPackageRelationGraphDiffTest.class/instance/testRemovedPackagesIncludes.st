running
testRemovedPackagesIncludes
	newRelationGraph clearPackages .
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff removedPackagesIncludes: (DAPackage on: (RPackageSet named: 'Collections-Abstract'))).
	self assert: (packageRelationGraphDiff removedPackagesIncludes: (DAPackage on: (RPackageSet named: 'Collections-Strings'))).
	