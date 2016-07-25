tests
testAddExtensionDependencies
	aPackageRelationGraph addExtensionDependencies: packageCollectionAbstract.
	"self assert: (packageStrings  dependentPackages includes: packageCollectionAbstract)."
	"self assert: (packageCollectionAbstract dependentPackages includes: packageKernel).
	self assert: (packageCollectionAbstract dependentPackages includes: packageRegexCore).
	self assert: (packageCollectionAbstract dependentPackages includes: packageCollectionsSequenceable)."

	
