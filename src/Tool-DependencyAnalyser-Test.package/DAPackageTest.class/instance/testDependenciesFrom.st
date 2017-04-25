tests
testDependenciesFrom
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	[ aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage ]
		on: PotentialOutDatedDependencyWarning
		do: [ :ex | ex resume "ignore" ].
	self
		assert:
			((aPackage dependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])