initialization
fromAnnouncement: anAnnouncement
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #categoryTitle put: anAnnouncement category title.
	self infoAt: #allFilteredCandidates put: anAnnouncement category allFilteredCandidates size.