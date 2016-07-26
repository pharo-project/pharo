tests
testReferenceDependenciesFrom
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage.
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency isReferenceDependency ]).
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])