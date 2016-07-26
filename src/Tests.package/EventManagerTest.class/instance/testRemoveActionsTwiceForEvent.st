running-remove actions
testRemoveActionsTwiceForEvent

	eventSource
		when: #anEvent send: #size to: eventListener;
		when: #anEvent send: #getTrue to: self;
		when: #anEvent: send: #fizzbin to: self.
	eventSource removeActionsForEvent: #anEvent.
	self assert: (eventSource hasActionForEvent: #anEvent) not.
	eventSource removeActionsForEvent: #anEvent.
	self assert: (eventSource hasActionForEvent: #anEvent) not.