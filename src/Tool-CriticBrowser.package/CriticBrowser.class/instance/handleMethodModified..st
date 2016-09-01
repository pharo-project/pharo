system annoucements
handleMethodModified: anAnnouncement

	self
		methodRemoved: anAnnouncement oldMethod;
		methodAdded: anAnnouncement newMethod
	
