notifying
notifyAll: anAnnouncement
	
	self isNotificationsAllowed ifTrue: [
		self announcer announce: anAnnouncement ]