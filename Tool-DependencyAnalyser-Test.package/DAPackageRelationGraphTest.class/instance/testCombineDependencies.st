tests
testCombineDependencies
	| aSet |
	aSet := Set new.
	aPackageRelationGraph computeStaticDependencies:packageCollectionAbstract.
	packageCollectionAbstract removeAllInternal.
	"we collect all the target PDPackage"
	packageCollectionAbstract dependencies do: [ :dependency |
		aSet add: dependency target.
	  ].
	aPackageRelationGraph combineDependencies: packageCollectionAbstract.
	aSet do: [ :each | self assert: (packageCollectionAbstract dependentPackages includes: each) ]
	

	
	
	
	
	