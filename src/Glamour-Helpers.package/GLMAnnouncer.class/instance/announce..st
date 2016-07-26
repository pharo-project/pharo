announce
announce: anAnnouncement
	(self isAnnouncementSuspended: anAnnouncement) ifTrue: [ ^ anAnnouncement asAnnouncement ].
	^ super announce: anAnnouncement