running-dependent action supplied arguments
testNoArgumentEventDependentSuppliedArgument

	eventSource when: #anEvent send: #add: to: eventListener with: 'boundValue'.
	eventSource triggerEvent: #anEvent.
	self should: [eventListener includes: 'boundValue']