initialization
fromAnnouncement: anAnnouncement
	super fromAnnouncement: anAnnouncement.
	self infoAt: #elementName putHashForText: anAnnouncement element asString.
	self infoAt: #elementType putHashForText: anAnnouncement element class name asString.