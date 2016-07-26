searching
lookUpForUndeterminateDependentPackagesFrom: parent
	| oldDependentPackages undeterminates |
	oldDependentPackages := (oldRelationGraph packageAt: parent packageName) dependentPackages.
	undeterminates := oldDependentPackages reject: [ :package | self removedDependentPackagesIncludes: package from: parent ].
	
	
	undeterminates do: [ :each | self addUndeterminateDependentPackage: each from: parent ].
