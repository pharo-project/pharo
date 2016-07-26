tests
testAddCollector2
	recorder addCollector: GTEventCollector new.
	self assert: recorder isDeliveryActive.