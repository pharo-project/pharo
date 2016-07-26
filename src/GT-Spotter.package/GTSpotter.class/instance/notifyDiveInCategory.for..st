notifying
notifyDiveInCategory: aProcessor for: anEventType

	self announce: (GTSpotterDiveInCategory new 
		category: aProcessor;
		eventType: anEventType)