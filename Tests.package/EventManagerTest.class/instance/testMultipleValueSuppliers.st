running-broadcast query
testMultipleValueSuppliers

	eventSource
		when: #needsValue
		send: #getFalse
		to: self.
	eventSource
		when: #needsValue
		send: #getTrue
		to: self.
	succeeded := eventSource triggerEvent: #needsValue.
	self should: [succeeded]