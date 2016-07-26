initialization
deliveryCycle
	delay := self standardDuration asDelay.
	self announceSuspendingDelivery.
	delay wait.
	delay := nil.
	self hasPermission ifTrue: [ self packAndDeliver: false ]