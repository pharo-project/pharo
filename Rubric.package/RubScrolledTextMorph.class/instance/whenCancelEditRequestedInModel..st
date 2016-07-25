event handling
whenCancelEditRequestedInModel: anAnnouncement
	self announcer announce: anAnnouncement.
	self cancel
