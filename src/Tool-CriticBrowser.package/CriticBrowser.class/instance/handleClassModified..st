system annoucements
handleClassModified: anAnnouncement

	self
		classRemoved: anAnnouncement oldClassDefinition;
		classAdded:   anAnnouncement newClassDefinition.
	