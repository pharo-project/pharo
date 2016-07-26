tests
testInitializeQueueWith	
	| dequeue |
	aPackageCycleDetection initializeQueueWith: packageA.
	self assert: self queue isNotNil.
	self assert: (self queue size = 1).
	dequeue := aPackageCycleDetection  dequeue.
	self assert: (dequeue = packageA).