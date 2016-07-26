tests
testNotDeliveredDataShouldBeResent
	"If the data are not received, we will resent them."
	collector1 category: #category1.
	collector1 add: event1; add: event2.
	"Server will not receive anything."
	server server delegate: ZnDispatcherDelegate new.
	recorder deliveryPreSuspendingAction: [ semaphore signal ].
	recorder addCollector: collector1.
	self assertSemaphore.
	recorder deliverNow.
	self assertSemaphore.
	self denyReceiverSemaphore.
	self assert: collector1 size equals: 0.
	self assert: receivedContents size equals: 0.
	self assert: recorder numberOfDeliveryData equals: 1.
	"Send again."
	server server delegate: delegate.
	self waitForDelivery.
	self denyReceiverSemaphore.
	self assert: receivedContents size equals: 1.
	self assert: recorder numberOfDeliveryData equals: 0.
