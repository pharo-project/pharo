running-dependent value
testReturnValueWithOneListener

	| value |
	eventSource
		when: #needsValue
		send: #yourself
		to: eventListener.
	value := eventSource triggerEvent: #needsValue.
	self should: [value == eventListener]