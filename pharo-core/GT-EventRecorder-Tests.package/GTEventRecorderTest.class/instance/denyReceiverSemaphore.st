asserting
denyReceiverSemaphore
	self assert: (receiverSemaphore waitTimeoutMSecs: 200)