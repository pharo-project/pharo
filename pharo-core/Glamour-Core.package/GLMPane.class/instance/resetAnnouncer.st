announcements
resetAnnouncer
	super resetAnnouncer.
	self presentations do: [ :each | each resetAnnouncer ]