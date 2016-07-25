running-remove actions
testRemoveActionsForEvent

	eventSource
		when: #anEvent send: #size to: eventListener;
		when: #anEvent send: #getTrue to: self;
		when: #anEvent: send: #fizzbin to: self.
	eventSource removeActionsForEvent: #anEvent.
	self shouldnt: [eventSource hasActionForEvent: #anEvent]