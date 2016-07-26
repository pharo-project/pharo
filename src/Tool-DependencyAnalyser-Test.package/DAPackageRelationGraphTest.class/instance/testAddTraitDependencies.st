tests
testAddTraitDependencies
	
	aPackageRelationGraph addTraitDependencies: packageCollectionAbstract.
	"Collections-Abstract has one dependency with himself, which includes TSortable"
	self assert: (packageCollectionAbstract dependentPackages includes: packageCollectionAbstract).