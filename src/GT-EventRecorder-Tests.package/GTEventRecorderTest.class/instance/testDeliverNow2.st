tests
testDeliverNow2
	"Ensure that liberated collector is removed."
	recorder deliveryPreSuspendingAction: [ semaphore signal ].
	collector1 add: 42.
	recorder addCollector: collector1.
	self assertSemaphore.
	recorder deliverNow.
	self assertSemaphore.
	self assert: collector1 size equals: 0.
	self assert: recorder numberOfCollectors equals: 0.