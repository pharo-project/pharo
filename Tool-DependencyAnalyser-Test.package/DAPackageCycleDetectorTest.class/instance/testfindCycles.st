tests
testfindCycles
	"the graph test is the SCC 1 from the paper"
	| aPDPackageRelation anArray |
	anArray := Array new: 5.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	anArray at: 4 put: packageD.
	anArray at: 5 put: packageE.
	(packageA) add: (DAPackageDependency from: (packageA) to: (packageB)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageC)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageA)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageD)).
	(packageC) add: (DAPackageDependency from: (packageC) to: (packageD)).
	(packageD) add: (DAPackageDependency from: (packageD) to: (packageE)).
	(packageD) add: (DAPackageDependency from: (packageD) to: (packageB)).
	(packageE) add: (DAPackageDependency from: (packageE) to: (packageA)).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: (aPackageCycleDetection  cycles anySatisfy: [ :cycle | ((cycle size = 2) or: (cycle size= 4)) or: (cycle size = 3) ]).
	