running-broadcast query
testSingleValueSupplier

	eventSource
		when: #needsValue
		send: #getTrue
		to: self.
	succeeded := eventSource triggerEvent: #needsValue.
	self should: [succeeded]