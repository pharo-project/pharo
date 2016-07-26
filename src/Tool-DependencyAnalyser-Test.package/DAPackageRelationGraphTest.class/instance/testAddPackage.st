tests
testAddPackage
	self assert: aPackageRelationGraph packages isEmpty.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: aPackageRelationGraph packages size = 1.
	aPackageRelationGraph addPackage: packageCollectionsSequenceable.
	self assert: aPackageRelationGraph packages size = 2.