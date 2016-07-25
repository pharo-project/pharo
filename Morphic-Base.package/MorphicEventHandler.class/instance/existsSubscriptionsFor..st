testing
existsSubscriptionsFor: anEvent
	^(subscriptions includesKey: anEvent) and: [ (subscriptions at: anEvent) notEmpty ]