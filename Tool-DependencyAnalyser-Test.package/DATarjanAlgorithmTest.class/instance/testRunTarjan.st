tests
testRunTarjan
	"test the Tarjan algorithm on a simple graph with 8 nodes"
	| aPDPackageRelation  |
	packageA add: (DAPackageDependency from: packageA to: packageB).
	packageB add: (DAPackageDependency from: packageB to: packageC).
	packageC add: (DAPackageDependency from: packageC to: packageD).
	packageD add: (DAPackageDependency from: packageD to: packageE).
	packageE add: (DAPackageDependency from: packageE to: packageA).
	packageC add: (DAPackageDependency from: packageC to: packageF).
	packageF add: (DAPackageDependency from: packageF to: packageG).
	packageG add: (DAPackageDependency from: packageG to: packageH).
	packageH add: (DAPackageDependency from: packageH to: packageG).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	tarjanPackage run: aPDPackageRelation.
	"we must find 3 sccs, five two and one nodes"
	self assert: (tarjanPackage stronglyConnectedComponents  size = 3).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 5 ]).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 2 ]).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 1 ]).
	
	
	
	

	
	