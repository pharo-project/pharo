event handling
whenTextSetInModel: anAnnouncement
	self announcer announce: anAnnouncement.
	self scrollPane setTextWith: self getTextFromModel