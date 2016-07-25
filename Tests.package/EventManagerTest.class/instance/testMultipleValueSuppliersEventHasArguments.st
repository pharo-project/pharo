running-broadcast query
testMultipleValueSuppliersEventHasArguments

	eventSource
		when: #needsValue:
		send: #getFalse:
		to: self.
	eventSource
		when: #needsValue:
		send: #getTrue:
		to: self.
	succeeded := eventSource triggerEvent: #needsValue: with: 'kolme'.
	self should: [succeeded]