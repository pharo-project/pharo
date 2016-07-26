tests
testRunTarjanWikipedia
	"test the Tarjan algorithm on the Wikipedia graph (page Tarjan's strongly connected components algorithm"
	| aPDPackageRelation |
	packageA add: (DAPackageDependency from: packageA to: packageB).
	packageB add: (DAPackageDependency from: packageB to: packageC).
	packageC add: (DAPackageDependency from: packageC to: packageA).
	packageD add: (DAPackageDependency from: packageD to: packageB).
	packageD add: (DAPackageDependency from: packageD to: packageC).
	packageD add: (DAPackageDependency from: packageD to: packageF).
	packageE add: (DAPackageDependency from: packageE to: packageC).
	packageE add: (DAPackageDependency from: packageE to: packageG).
	packageF add: (DAPackageDependency from: packageF to: packageD).
	packageF add: (DAPackageDependency from: packageF to: packageE).
	packageG add: (DAPackageDependency from: packageG to: packageE).
	"packageG add: (PDPackageDependency from: packageG to: packageF)."
	packageH add: (DAPackageDependency from: packageH to: packageH).
	packageH add: (DAPackageDependency from: packageH to: packageF).
	packageH add: (DAPackageDependency from: packageH to: packageG).
	
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	tarjanPackage run: aPDPackageRelation.
	"we must find 4 sccs"
	self assert: (tarjanPackage stronglyConnectedComponents  size = 4).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 3 ]).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 2 ]).
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :each | each packages size = 1 ]).