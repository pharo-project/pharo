notifying
notifyActOn: anObject for: anEventType

	self announce: (GTSpotterActOn new 
		element: anObject;
		eventType: anEventType)