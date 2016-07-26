tests
testAddInheritanceDependencies
	aPackageRelationGraph addInheritanceDependencies: packageCollectionAbstract.
	"Collections-Abstract has one dependency with Error and Object by Inheritance"
	self assert: (packageCollectionAbstract dependentPackages includes: packageKernel).