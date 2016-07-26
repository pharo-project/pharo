tests
testEnqueue
	aPackageCycleDetection initializeQueueWith: packageA.
		self assert: (self queue size = 1).
	aPackageCycleDetection enqueue: packageB.
	self assert: (self queue size = 2)
	