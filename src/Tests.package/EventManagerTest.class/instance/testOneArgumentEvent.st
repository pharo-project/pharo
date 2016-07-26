running-dependent action
testOneArgumentEvent

	eventSource when: #anEvent: send: #add: to: eventListener.
	eventSource triggerEvent: #anEvent: with: 9.
	self should: [eventListener includes: 9]