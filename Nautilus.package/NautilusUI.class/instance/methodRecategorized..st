system announcements
methodRecategorized: anAnnouncement
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	(self interestedInClass: anAnnouncement methodClass)
		ifTrue: [self updateCategoryAndMethodList ]