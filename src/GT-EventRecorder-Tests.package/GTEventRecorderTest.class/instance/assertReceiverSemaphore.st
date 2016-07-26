asserting
assertReceiverSemaphore
	self deny: (receiverSemaphore waitTimeoutMSecs: 200)