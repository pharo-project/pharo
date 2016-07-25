tests
testAddPackages
	| array |
	self assert: aPackageRelationGraph packages isEmpty.
	array := Array with: packageCollectionAbstract with: packageCollectionsSequenceable with: packageKernel.
	aPackageRelationGraph addPackages: array.
	self assert: aPackageRelationGraph packages size = 3