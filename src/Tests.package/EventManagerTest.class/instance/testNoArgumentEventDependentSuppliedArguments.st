running-dependent action supplied arguments
testNoArgumentEventDependentSuppliedArguments

	eventSource 
		when: #anEvent 
		send: #addArg1:addArg2: 
		to: self 
		withArguments: #('hello' 'world').
	eventSource triggerEvent: #anEvent.
	self should: [(eventListener includes: 'hello') and: [eventListener includes: 'world']]