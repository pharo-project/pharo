tests
testReferenceDependenciesFrom
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	[ aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage ]
	on: PotentialOutDatedDependencyWarning
	do: [ :ex | ex resume "ignore" ].
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency isReferenceDependency ]).
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])