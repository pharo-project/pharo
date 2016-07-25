tests
testAddVisitedNodes
	aPackageCycleDetection initializeVisitedNodes.
	aPackageCycleDetection addVisitedNodes: packageA.
	self assert: (aPackageCycleDetection visitedNodes size = 1).
	self assert: (aPackageCycleDetection visitedNodes includes: packageA)