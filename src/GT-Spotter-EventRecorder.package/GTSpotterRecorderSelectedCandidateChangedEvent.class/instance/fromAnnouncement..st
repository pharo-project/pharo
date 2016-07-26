initialization
fromAnnouncement: anAnnouncement
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #elementName putHashForText: anAnnouncement selected asString.
	self infoAt: #elementType putHashForText: anAnnouncement selected class name asString.
