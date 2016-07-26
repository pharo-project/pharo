tests
testPdPackageFrom
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: ((aPackageRelationGraph pdPackageFrom: Collection) = packageCollectionAbstract )