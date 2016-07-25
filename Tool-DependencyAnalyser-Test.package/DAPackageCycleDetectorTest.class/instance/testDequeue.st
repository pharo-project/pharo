tests
testDequeue
	| dequeue |
	aPackageCycleDetection initializeQueueWith: packageA.
	dequeue := aPackageCycleDetection dequeue.
	self assert: (dequeue = packageA).
	self assert: self queue isEmpty