running-dependent action
testBlockReceiverOneArg
	eventSource when: #anEvent: evaluate:[:arg1| eventListener add: arg1].
	eventSource triggerEvent: #anEvent: with: 9.
	self should: [eventListener includes: 9]