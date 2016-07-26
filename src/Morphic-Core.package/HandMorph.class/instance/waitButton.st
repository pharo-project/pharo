events-processing
waitButton
	self captureEventsUntil: [:evt | self anyButtonPressed ]
