tests
testDeliverNow3
	"Ensure that collector with registerd holder is not removed."
	recorder deliveryPreSuspendingAction: [ semaphore signal ].
	collector1 add: 42.
	collector1 occupant: self.
	recorder addCollector: collector1.
	self assertSemaphore.
	recorder deliverNow.
	self assertSemaphore.
	self assert: collector1 size equals: 0.
	self assert: recorder numberOfCollectors equals: 1.