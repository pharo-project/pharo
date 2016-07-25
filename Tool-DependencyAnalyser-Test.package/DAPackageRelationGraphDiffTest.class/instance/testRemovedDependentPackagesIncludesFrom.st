running
testRemovedDependentPackagesIncludesFrom
	| dependentPackages package |
	package := newRelationGraph seenPackages first.
	dependentPackages := package dependentPackages.
	
	package clearDependencies.
	
	packageRelationGraphDiff make.
			
	self assert: (packageRelationGraphDiff removedDependentPackagesIncludes: dependentPackages anyOne from: package).
			
	
	
