notifying
notifyDiveIn: anObject for: anEventType

	self announce: (GTSpotterDiveInElement new 
		element: anObject;
		eventType: anEventType )