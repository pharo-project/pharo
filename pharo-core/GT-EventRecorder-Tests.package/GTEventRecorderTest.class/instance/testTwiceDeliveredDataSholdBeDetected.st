tests
testTwiceDeliveredDataSholdBeDetected
	"It happens regularly that the server receives the same data several times.
	We need to be sure that we can detect it easily."
	| bundles data |
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
	bundles := recorder delivery bundles.
	self assert: bundles size equals: 1.
	self assert: recorder numberOfDeliveryData equals: 0.
	"Send twice."
	server server delegate: delegate.
	recorder delivery deliver: bundles first signalProgress: false.
	recorder delivery deliver: bundles first signalProgress: false.
	self waitForDelivery.
	self waitForDelivery.
	self denyReceiverSemaphore.
	self assert: receivedContents size equals: 2.
	self assert: recorder numberOfDeliveryData equals: 0.
	"Check that we can detect duplication"
	data := OrderedCollection new.
	receivedContents flush: [ :each | data addLast: each ].
	self assert: data first second contents equals: data second second contents.