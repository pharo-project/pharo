tests
testSuccessors
	| aCollection |
	"A -> B, C, D ; B -> D, C"
	aCollection := OrderedCollection new.
	aCollection add: packageA.
	aCollection add: packageB.
	aCollection add: packageC.
	aCollection add: packageD.
	packageA add: (DAPackageDependency from:packageA to: packageB).
	packageA add: (DAPackageDependency from:packageA to: packageC).
	packageA add: (DAPackageDependency from:packageA to: packageD).
	packageB add: (DAPackageDependency from:packageB to: packageC).
	packageB add: (DAPackageDependency from:packageB to: packageD).
	aCollection do: [ :package | aPackageRelationGraph addPackage: package ].
	self assert: ((aPackageRelationGraph successors: (aCollection at: 1)) size = 3).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 4)) isEmpty).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 2)) size = 2).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 3)) isEmpty).