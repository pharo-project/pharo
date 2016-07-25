events
addSubscription: aSubscription toEvent: eventName
	(subscriptions includesKey: eventName)
		ifFalse: [ subscriptions at: eventName put: Set new. ].
	(subscriptions at: eventName)
		add: aSubscription.