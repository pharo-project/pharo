tests
testPredecessors
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
	self assert: ((aPackageRelationGraph predecessors: packageC) size = 2).
	self assert: ((aPackageRelationGraph predecessors: (aCollection at: 1)) isEmpty).
	self assert: ((aPackageRelationGraph predecessors: packageB) size = 1).
	self assert: ((aPackageRelationGraph predecessors: packageD) size = 2).