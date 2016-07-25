announcements
resetAnnouncer
	super resetAnnouncer.
	self panes do: [:each | each resetAnnouncer ]