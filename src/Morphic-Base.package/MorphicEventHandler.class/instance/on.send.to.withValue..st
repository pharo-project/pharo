initialization
on: eventName send: selector to: recipient withValue: value
	
	selector numArgs = 3 ifFalse:
		[self error: 'Warning: value parameters are passed as first of 3 arguments'].
	self
		addSubscription: (MorphEventSubscription on: eventName send: selector to: recipient withValue: value)
		toEvent: eventName.
	
