event handling
whenTextUpdatedInModel: anAnnouncement
	self announcer announce: anAnnouncement.
	self updateTextWith: self getTextFromModel
