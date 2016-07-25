tests
testNoOutgoingDependenciesAfterTarjan
	"test if we have 0 outgoing dependencies in each SCC after the algorithm"

	| aRelation aCollection |
	aCollection := OrderedCollection
		withAll:
			(RPackageOrganizer default packages
				select: [ :package | '*Collections*' match: package packageName asString ]
				thenCollect: [ :package | package packageName ]).
	aRelation := DAPackageRelationGraph
		onPackages: (aCollection collect: [ :each | DAPackage on: (RPackageSet named: each) ]).
	aRelation
		computeStaticDependencies;
		removeInternalDependencies;
		removeOutgoingDependencies;
		combineDependencies.
	tarjanPackage run: aRelation.
	self assert: (tarjanPackage stronglyConnectedComponents anySatisfy: [ :scc | scc outgoing isEmpty ])