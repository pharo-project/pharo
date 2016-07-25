tests
testFindCyclesOne
	"we must find 1 cycles in this acyclic graph"
	
	| aPDPackageRelation anArray |
	anArray := Array new: 3.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	(packageA) add: (DAPackageDependency from: (packageA) to: (packageB)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageC)).
	(packageC) add: (DAPackageDependency from: (packageC) to: (packageA)).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: (aPackageCycleDetection  cycles size = 1).