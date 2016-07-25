tests
testDependenciesFrom
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage.
	self
		assert:
			((aPackage dependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])