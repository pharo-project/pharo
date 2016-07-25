announcements
methodModified: anAnnouncement
	"this method forces the announcement to be handled in the UI process"
	UIManager default defer:  [	
		self handleMethodModified: anAnnouncement ]