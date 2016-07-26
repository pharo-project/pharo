running-dependent action
testNoArgumentEvent

	eventSource when: #anEvent send: #heardEvent to: self.
	eventSource triggerEvent: #anEvent.
	self should: [succeeded]