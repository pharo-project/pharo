initialization
initialize
	super initialize.
	self
		category: #qualityAssistantEvents;
		updateAnnouncementBlock: [ :aGTEventAnnouncement |
			aGTEventAnnouncement at: #qaEventFormatVersion put: 1.1 ]