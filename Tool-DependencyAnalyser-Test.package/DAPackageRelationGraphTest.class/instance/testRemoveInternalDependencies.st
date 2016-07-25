tests
testRemoveInternalDependencies
	aPackageRelationGraph computeStaticDependencies.
	aPackageRelationGraph removeInternalDependencies.
	
	aPackageRelationGraph packages do: [ :value |
				value dependencies do: 
					[ :dependency |
					self assert: (dependency isExternal).
					 ] ].