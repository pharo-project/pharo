announce
announce: anAnnouncement
	self isSuspended 
		ifFalse: [ 
			self private announce: anAnnouncement.
			super announce: anAnnouncement ]
		ifTrue:[
			storedAnnouncements ifNotNil:[ storedAnnouncements add: anAnnouncement ]
		].