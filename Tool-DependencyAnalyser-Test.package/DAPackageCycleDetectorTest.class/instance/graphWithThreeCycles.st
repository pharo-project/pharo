utility
graphWithThreeCycles
	| anArray |
	anArray := Array new: 3.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	packageA add: (DAPackageDependency from: packageA to: packageB).
	packageB add: (DAPackageDependency from: packageB to: packageA).
	packageA add: (DAPackageDependency from: packageA to: packageC).
	packageC add: (DAPackageDependency from: packageB to: packageA).
	packageC add: (DAPackageDependency from: packageB to: packageB).
	^ DAPackageRelationGraph onPackages: anArray