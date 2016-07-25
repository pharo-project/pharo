running-dependent action
testTwoArgumentEvent

	eventSource when: #anEvent:info: send: #addArg1:addArg2: to: self.
	eventSource triggerEvent: #anEvent:info: withArguments: #( 9 42 ).
	self should: [(eventListener includes: 9) and: [eventListener includes: 42]]