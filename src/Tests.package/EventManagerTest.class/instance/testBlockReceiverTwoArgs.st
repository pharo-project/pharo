running-dependent action
testBlockReceiverTwoArgs
	eventSource when: #anEvent:info: evaluate:[:arg1 :arg2| self addArg1: arg1 addArg2: arg2].
	eventSource triggerEvent: #anEvent:info: withArguments: #( 9 42 ).
	self should: [(eventListener includes: 9) and: [eventListener includes: 42]]