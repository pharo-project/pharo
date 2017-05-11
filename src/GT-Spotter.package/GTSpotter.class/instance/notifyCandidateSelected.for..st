notifying
notifyCandidateSelected: anObject for: anEventType

	self announce: (GTSpotterSelectedCandidateChanged new 
		select: anObject;
		eventType: anEventType)