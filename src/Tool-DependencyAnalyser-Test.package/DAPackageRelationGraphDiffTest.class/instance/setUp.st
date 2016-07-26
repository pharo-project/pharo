running
setUp
	| packageCollectionsAbstract packageCollectionsString |
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageCollectionsAbstract := DAPackage on: (RPackageSet named: 'Collections-Abstract').
	packageCollectionsString := DAPackage on: (RPackageSet named: 'Collections-Strings').
	oldRelationGraph := DAPackageRelationGraph onPackages: (Array with: packageCollectionsAbstract with: packageCollectionsString ).
	newRelationGraph := DAPackageRelationGraph
		onPackages: (Array with: (DAPackage on: (RPackageSet named: 'Collections-Abstract')) with: (DAPackage on: (RPackageSet named: 'Collections-Strings'))).
	oldRelationGraph build.
	newRelationGraph build.
	

	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph