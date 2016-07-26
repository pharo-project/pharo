announcement handling
whenKeystroke: anAnnouncement
	anAnnouncement event keyCharacter = Character escape
		ifTrue: [ escapeBlock value.
			self closeEditor ]