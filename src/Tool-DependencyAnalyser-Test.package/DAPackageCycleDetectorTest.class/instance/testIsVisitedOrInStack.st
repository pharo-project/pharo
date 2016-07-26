tests
testIsVisitedOrInStack
	aPackageCycleDetection initializeVisitedNodes.
	aPackageCycleDetection initializeQueueWith: packageA.
	self assert: (aPackageCycleDetection isVisitedOrInQueue: packageA).
	aPackageCycleDetection dequeue.
	self deny: (aPackageCycleDetection isVisitedOrInQueue: packageA).
	aPackageCycleDetection addVisitedNodes: packageA.
	self assert: (aPackageCycleDetection isVisitedOrInQueue: packageA)