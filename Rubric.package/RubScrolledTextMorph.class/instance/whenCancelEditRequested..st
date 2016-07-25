event handling
whenCancelEditRequested: anAnnouncement
	self announcer announce: anAnnouncement.
	self cancel
