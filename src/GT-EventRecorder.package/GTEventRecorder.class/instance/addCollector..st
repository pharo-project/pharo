adding
addCollector: aGTEventCollector
	self activateDeliveryIfAvailable.
	^ collectors add: aGTEventCollector