tests
testRemoveCollector2
	recorder addCollector: collector1.
	self assert: recorder isDeliveryActive.
	recorder removeCollector: collector1.
	self deny: recorder isDeliveryActive.