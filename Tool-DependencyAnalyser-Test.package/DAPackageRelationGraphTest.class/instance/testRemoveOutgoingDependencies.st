tests
testRemoveOutgoingDependencies
	aPackageRelationGraph computeStaticDependencies.
	aPackageRelationGraph removeOutgoingDependencies.
	
	aPackageRelationGraph packages do: [ :package |
			package dependencies do: [ :dependency |
				self assert: (dependency target isIncluded)
				]
		 ].