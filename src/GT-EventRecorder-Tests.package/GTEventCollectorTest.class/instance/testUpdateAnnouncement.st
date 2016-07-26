tests
testUpdateAnnouncement
	| announcement announcement2 |
	collector updateAnnouncementBlock: [ :anAnnouncement | 
		anAnnouncement at: #testUpdateAnnouncement put: 42 ].
	announcement := GTEventAnnouncement new.
	self assert: (announcement at: #testUpdateAnnouncement) isNil.
	announcement2 := collector updateAnnouncement: announcement.
	self assert: announcement == announcement2.
	self assert: (announcement at: #testUpdateAnnouncement) equals: 42.