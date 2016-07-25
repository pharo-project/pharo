system announcements
packageChanged: anAnnouncement
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView