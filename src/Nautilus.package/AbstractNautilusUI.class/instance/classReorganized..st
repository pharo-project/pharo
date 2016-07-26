system announcements
classReorganized: anAnnouncement
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	(self selectedClass = anAnnouncement classReorganized or: [ anAnnouncement classReorganized users includes: self selectedClass  ])
		ifTrue: [ self updatePackageGroupAndClassList ]