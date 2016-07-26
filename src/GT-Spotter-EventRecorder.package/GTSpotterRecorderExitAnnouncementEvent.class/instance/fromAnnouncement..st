initialization
fromAnnouncement: anAnnouncement
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.

