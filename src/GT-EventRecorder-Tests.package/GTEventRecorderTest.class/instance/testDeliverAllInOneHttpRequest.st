tests
testDeliverAllInOneHttpRequest
	collector1 category: #category1.
	collector2 category: #category2.
	collector1 add: event1; add: event2.
	collector2 add: event3; add: event4.
	recorder deliveryPreSuspendingAction: [ semaphore signal ].
	recorder addCollector: collector1.
	recorder addCollector: collector2.
	self assertSemaphore.
	self waitForDelivery.
	self denyReceiverSemaphore.
	self assert: collector1 size equals: 0.
	self assert: collector2 size equals: 0.
	self assert: receivedContents size equals: 1.
