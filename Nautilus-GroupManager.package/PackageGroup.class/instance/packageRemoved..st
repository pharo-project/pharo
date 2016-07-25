announcements
packageRemoved: anAnnouncement
	packages remove: anAnnouncement package ifAbsent: [  ].
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)