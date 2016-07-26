tests
testAddCollector3
	recorder addCollector: collector1.
	collector1 add: event1.
	collector1 add: event2.
	recorder deliveryPreSuspendingAction: [ semaphore signal ].
	self assertSemaphore.
	self waitForDelivery.
	self assert: collector1 size equals: 0.
	self assert: receivedContents size equals: 1.
	