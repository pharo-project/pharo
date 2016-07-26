event handling
whenPrimarySelectionUpdatedInModel: anAnnouncement
	self announcer announce: anAnnouncement.
	self setTextAreaSelection: self getSelectionFromModel.

