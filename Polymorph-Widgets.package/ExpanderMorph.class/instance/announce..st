operations
announce: anAnnouncement
	"see Announcements packages.
	No need to announce if no subscribers."
	
	announcer ifNotNil: [announcer announce: anAnnouncement]