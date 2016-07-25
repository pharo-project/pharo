announcement handling
whenEditorDeleted: anAnnouncement
	anAnnouncement morph activeHand removeEventListener: self.
	anAnnouncement morph announcer unsubscribe: self.
	editor := nil