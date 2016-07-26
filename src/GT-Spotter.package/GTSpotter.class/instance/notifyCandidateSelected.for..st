notifying
notifyCandidateSelected: anObject for: anEventType

	self announce: (GTSpotterSelectedCandidateChanged new 
		selected: anObject;
		eventType: anEventType)