event handling
whenEditsStateChangedInModel: anAnnouncement
	self announcer announce: anAnnouncement.
	self basicHasUnacceptedEdits: self model hasUnacceptedEdits.
	self changed
