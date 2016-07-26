tests
testFindCyclesThreeNodes
	| aPDPackageRelation |
	aPDPackageRelation := self graphWithThreeCycles.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: aPackageCycleDetection cycles size = 3