tests
testDependenciesFrom
	| aCollection |
	aCollection := OrderedCollection new.
	aCollection add: packageA.
	aCollection add: packageB.
	aCollection add: packageC.
	(aCollection at:1) add: (DAPackageDependency from: packageA to: packageB).
	(aCollection at:1) add: (DAPackageDependency from:packageA to: packageC).
	(aCollection at:2) add: (DAPackageDependency from: packageB to: packageC).
	aCollection do: [ :package | aPackageRelationGraph addPackage: package ].
	self assert: ((aPackageRelationGraph dependenciesFrom:'A') size = 2).
	self assert: ((aPackageRelationGraph dependenciesFrom:'B') size = 1).
	self assert: ((aPackageRelationGraph dependenciesFrom:'C') isEmpty).