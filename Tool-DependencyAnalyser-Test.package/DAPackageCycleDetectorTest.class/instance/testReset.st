tests
testReset
	| aPDPackageRelation |
	aPDPackageRelation := self graphWithThreeCycles.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: aPackageCycleDetection cycles size = 3.
	aPackageCycleDetection reset.
	self assert: aPackageCycleDetection cycles isEmpty