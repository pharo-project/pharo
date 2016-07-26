accessing
forbid: aPane toAnnounce: anAnnouncement
	(self forbiddenAnnouncements at: aPane ifAbsentPut: [Set new])
		add: anAnnouncement