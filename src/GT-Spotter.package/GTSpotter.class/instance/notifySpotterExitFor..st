notifying
notifySpotterExitFor: anEventType

	self announce: (GTSpotterExitAnnouncement new
		eventType: anEventType)