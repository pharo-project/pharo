tests
testRemovePackage
	self assert: aPackageRelationGraph packages isEmpty.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: aPackageRelationGraph packages size = 1.
	aPackageRelationGraph removePackage: packageCollectionAbstract packageName.
	self assert: aPackageRelationGraph packages isEmpty