running-dependent value
testReturnValueWithManyListeners

	| value newListener |
	newListener := 'busybody'.
	eventSource
		when: #needsValue
		send: #yourself
		to: eventListener.
	eventSource
		when: #needsValue
		send: #yourself
		to: newListener.
	value := eventSource triggerEvent: #needsValue.
	self should: [value == newListener]