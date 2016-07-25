initialization
on: eventName send: selector to: recipient
	self
		addSubscription: (MorphEventSubscription on: eventName send: selector to: recipient)
		toEvent: eventName.