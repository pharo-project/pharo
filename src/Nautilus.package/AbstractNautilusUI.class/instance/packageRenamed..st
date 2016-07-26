system announcements
packageRenamed: anAnnouncement
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView