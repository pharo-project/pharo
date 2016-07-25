searching
lookUpForUndeterminatePackages
	| undeterminates |
	undeterminates := oldRelationGraph seenPackages reject: [ :package | self removedPackagesIncludes: package ].
	undeterminates do: [ :each | self addUndeterminatePackage: each ]