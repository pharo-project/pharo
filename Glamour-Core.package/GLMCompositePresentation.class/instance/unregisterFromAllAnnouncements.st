announcements
unregisterFromAllAnnouncements
	super unregisterFromAllAnnouncements.
	self presentations do: [:each | each unregisterFromAllAnnouncements ]